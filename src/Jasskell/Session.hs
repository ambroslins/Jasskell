module Jasskell.Session
  ( SessionId,
    Nickname (toText),
    Session (nickname),
    SessionRegistry,
    newRegistry,
    newSession,
    getSession,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64
import Data.ByteString.Char8 qualified as BS
import Data.IORef (IORef, newIORef, readIORef)
import Data.IntMap.Coercible (IntMap)
import Data.IntMap.Coercible qualified as IntMap
import Data.String (IsString)
import Data.Text (Text)
import GHC.IORef (atomicModifyIORef'_)
import Jasskell.Id (Id)
import Jasskell.Id qualified as Id
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteStrict)
import Web.Twain qualified as Twain

type SessionId = Id Session

newtype Nickname = Nickname {toText :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

data Session = Session
  { sessionId :: SessionId,
    secretHash :: Digest SHA256,
    nickname :: Nickname
  }
  deriving (Show)

newtype SessionRegistry = SessionRegistry (IORef (IntMap SessionId Session))

newRegistry :: (MonadIO m) => m SessionRegistry
newRegistry = liftIO $ SessionRegistry <$> newIORef IntMap.empty

newSession :: (MonadIO m) => SessionRegistry -> Nickname -> m (Session, SetCookie)
newSession (SessionRegistry sessionsRef) nickname = liftIO $ do
  sessionId <- Id.new
  secret <- getRandomBytes @IO @ByteString 32
  let !session = Session {sessionId, secretHash = hash secret, nickname}
  void $ atomicModifyIORef'_ sessionsRef (IntMap.insert sessionId session)
  pure (session, makeSessionCookie sessionId secret)

makeSessionCookie :: SessionId -> ByteString -> SetCookie
makeSessionCookie sessionId secret =
  defaultSetCookie
    { setCookieName = "session",
      setCookieValue =
        BS.intercalate
          "."
          [ Id.encodeByteString sessionId,
            Base64.encode secret
          ],
      setCookieHttpOnly = True,
      setCookieSecure = False, -- TODO: fix for debugging
      setCookieSameSite = Just sameSiteStrict
    }

getSession :: SessionRegistry -> Twain.ResponderM (Maybe Session)
getSession (SessionRegistry sessionsRef) =
  Twain.cookieParamMaybe "session" >>= \case
    Nothing -> pure Nothing
    Just value -> case parseSessionCookie value of
      Left _ -> pure Nothing
      Right (sessionId, secret) -> do
        sessions <- liftIO $ readIORef sessionsRef
        case IntMap.lookup sessionId sessions of
          Nothing -> pure Nothing
          Just session
            | hash secret == session.secretHash -> pure $ Just session
            | otherwise -> pure Nothing

parseSessionCookie :: ByteString -> Either String (SessionId, ByteString)
parseSessionCookie value = case BS.split '.' value of
  [idBase64, secretBase64] -> do
    sessionId <- Id.decodeByteString idBase64
    secret <- Base64.decode secretBase64
    pure (sessionId, secret)
  _ -> Left "invalid session cookie"

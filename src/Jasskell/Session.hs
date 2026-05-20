module Jasskell.Session
  ( SessionId,
    Nickname (toText),
    Session (id, nickname),
    Registry,
    newRegistry,
    cookieName,
    new,
    get,
  )
where

import Control.Monad (unless, void)
import Control.Monad.Except (throwError)
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
  deriving newtype (Eq, Ord, Show, IsString, Twain.ParsableParam)

data Session = Session
  { id :: SessionId,
    secretHash :: Digest SHA256,
    nickname :: Nickname
  }
  deriving (Show)

newtype Registry = Registry (IORef (IntMap SessionId Session))

newRegistry :: (MonadIO m) => m Registry
newRegistry = liftIO $ Registry <$> newIORef IntMap.empty

new :: (MonadIO m) => Registry -> Nickname -> m (Session, SetCookie)
new (Registry sessionsRef) nickname = liftIO $ do
  sessionId <- Id.new
  secret <- getRandomBytes @IO @ByteString 32
  let !session = Session {id = sessionId, secretHash = hash secret, nickname}
  void $ atomicModifyIORef'_ sessionsRef (IntMap.insert sessionId session)
  pure (session, makeSessionCookie sessionId secret)

cookieName :: (IsString s) => s
cookieName = "session"

makeSessionCookie :: SessionId -> ByteString -> SetCookie
makeSessionCookie sessionId secret =
  defaultSetCookie
    { setCookieName = cookieName,
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

get :: (MonadIO m) => Registry -> ByteString -> m (Either String Session)
get (Registry sessionsRef) cookie = do
  sessions <- liftIO $ readIORef sessionsRef
  pure $ do
    (sessionId, secret) <- parseSessionCookie cookie
    session <-
      maybe (throwError $ "session not found: " <> show sessionId) pure $
        IntMap.lookup sessionId sessions
    unless (hash secret == session.secretHash) $ throwError "secret hash mismatch"
    pure session

parseSessionCookie :: ByteString -> Either String (SessionId, ByteString)
parseSessionCookie cookie = case BS.split '.' cookie of
  [idBase64, secretBase64] -> do
    sessionId <- Id.decodeByteString idBase64
    secret <- Base64.decode secretBase64
    pure (sessionId, secret)
  _ -> Left "invalid session cookie"

{-# LANGUAGE QuasiQuotes #-}

module Jasskell.Session
  ( SessionId,
    Nickname (toText),
    Session (id, nickname),
    SessionError (..),
    cookieName,
    new,
    get,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Crypto.Hash (Digest, digestFromByteString, hash)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64
import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.Profunctor (lmap)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH (maybeStatement, resultlessStatement)
import Jasskell.App
import Jasskell.Id (Id)
import Jasskell.Id qualified as Id
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteStrict)
import Web.Twain qualified as Twain

type SessionId = Id Session

newtype Nickname = Nickname {toText :: Text}
  deriving newtype (Eq, Ord, Show, IsString, Twain.ParsableParam)

data Session = Session
  { id :: SessionId,
    nickname :: Nickname
  }
  deriving (Show)

new :: (MonadIO m) => Nickname -> AppT m (Session, SetCookie)
new nickname = do
  sessionId <- Id.new
  secret <- liftIO $ getRandomBytes 32
  let !secretSHA256 = hash @ByteString @SHA256 secret
      !session = Session {id = sessionId, nickname}
  useDB $ Hasql.statement (sessionId, nickname, secretSHA256) insertSession
  pure (session, makeSessionCookie sessionId secret)

insertSession :: Hasql.Statement (SessionId, Nickname, Digest SHA256) ()
insertSession =
  lmap
    ( \(sessionId, nickname, secretSHA256) ->
        (Id.toInt64 sessionId, nickname.toText, convert secretSHA256)
    )
    [resultlessStatement|
      insert into sessions (id, nickname, secret_sha256)
      values ($1::int8, $2::text, $3::bytea)
    |]

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
      setCookieSameSite = Just sameSiteStrict,
      setCookiePath = Just "/"
    }

data SessionError
  = InvalidSessionCookie String
  | SessionNotFound SessionId
  | SessionSecretHashMismatch
  deriving (Eq, Show)

get :: (MonadIO m) => ByteString -> AppT m (Either SessionError Session)
get cookie = runExceptT $ do
  (sessionId, secret) <- case parseSessionCookie cookie of
    Left e -> throwError $ InvalidSessionCookie e
    Right x -> pure x
  (nickname, secretSHA256) <-
    lift (useDB $ Hasql.statement sessionId selectSessionById)
      >>= \case
        Nothing -> throwError $ SessionNotFound sessionId
        Just x -> pure x
  unless (hash secret == secretSHA256) $ throwError SessionSecretHashMismatch
  pure $! Session {id = sessionId, nickname}

selectSessionById :: Hasql.Statement SessionId (Maybe (Nickname, Digest SHA256))
selectSessionById =
  lmap Id.toInt64 $
    Hasql.refineResult
      ( \case
          Nothing -> Right Nothing
          Just (nickname, secretHash) -> case digestFromByteString secretHash of
            Nothing -> Left $ "invalid sha256 length: " <> Text.show (BS.length secretHash)
            Just secretSHA256 -> Right $ Just (coerce nickname, secretSHA256)
      )
      [maybeStatement|
        select nickname::text, secret_sha256::bytea
        from sessions
        where id = $1::int8
      |]

parseSessionCookie :: ByteString -> Either String (SessionId, ByteString)
parseSessionCookie cookie = case BS.split '.' cookie of
  [idBase64, secretBase64] -> do
    sessionId <- Id.decodeByteString idBase64
    secret <- Base64.decode secretBase64
    pure (sessionId, secret)
  _ -> Left "invalid session cookie"

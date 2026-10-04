{-# LANGUAGE QuasiQuotes #-}

module Jasskell.Player
  ( PlayerId,
    Nickname (..),
    parseNickname,
    Player (..),
    SessionError (..),
    sessionCookieName,
    newSession,
    fromSessionCookie,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (Digest, digestFromByteString, hash)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.Coerce (coerce)
import Data.Profunctor (lmap)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH (maybeStatement, resultlessStatement)
import Jasskell.Database qualified as DB
import Jasskell.Id (Id)
import Jasskell.Id qualified as Id
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteStrict)

type PlayerId = Id Player

newtype Nickname = Nickname {toText :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

parseNickname :: Text -> Either String Nickname
parseNickname name
  | len < 2 = Left "name too short"
  | len > 20 = Left "name too long"
  | Just c <- Text.find (not . allowed) name = Left $ "invalid char: " <> show c
  | any Text.null (Text.split isSep name) = Left "missplaced separator"
  | otherwise = Right $ Nickname name
  where
    len = Text.length name
    allowed c = Char.isAlphaNum c || isSep c
    isSep c = c == '-' || c == '_'

data Player = Player
  { id :: PlayerId,
    nickname :: Nickname
  }
  deriving (Eq, Show)

newSession :: DB.Pool -> Nickname -> IO (Player, SetCookie)
newSession db nickname = do
  sessionId <- Id.new
  secret <- liftIO $ getRandomBytes 32
  let !secretSHA256 = hash @ByteString @SHA256 secret
      !player = Player {id = sessionId, nickname}
  DB.use db $ Hasql.statement (sessionId, nickname, secretSHA256) insertPlayer
  pure (player, makeSessionCookie sessionId secret)

insertPlayer :: Hasql.Statement (PlayerId, Nickname, Digest SHA256) ()
insertPlayer =
  lmap
    ( \(playerId, nickname, secretSHA256) ->
        (Id.toInt64 playerId, nickname.toText, convert secretSHA256)
    )
    [resultlessStatement|
      insert into players (player_id, nickname, secret_sha256)
      values ($1::int8, $2::text, $3::bytea)
    |]

sessionCookieName :: (IsString s) => s
sessionCookieName = "session"

makeSessionCookie :: PlayerId -> ByteString -> SetCookie
makeSessionCookie playerId secret =
  defaultSetCookie
    { setCookieName = sessionCookieName,
      setCookieValue =
        BS.intercalate
          "."
          [ Id.encodeByteString playerId,
            Base64.encode secret
          ],
      setCookieHttpOnly = True,
      setCookieSecure = False, -- TODO: fix for debugging
      setCookieSameSite = Just sameSiteStrict,
      setCookiePath = Just "/"
    }

data SessionError
  = InvalidSessionCookie String
  | PlayerNotFound PlayerId
  | SessionSecretHashMismatch
  deriving (Eq, Show)

fromSessionCookie :: DB.Pool -> ByteString -> IO (Either SessionError Player)
fromSessionCookie db cookie = runExceptT $ do
  (playerId, secret) <- case parseSessionCookie cookie of
    Left e -> throwError $ InvalidSessionCookie e
    Right x -> pure x
  (nickname, secretSHA256) <-
    liftIO (DB.use db $ Hasql.statement playerId selectPlayerById)
      >>= \case
        Nothing -> throwError $ PlayerNotFound playerId
        Just x -> pure x
  unless (hash secret == secretSHA256) $ throwError SessionSecretHashMismatch
  pure $! Player {id = playerId, nickname}

selectPlayerById :: Hasql.Statement PlayerId (Maybe (Nickname, Digest SHA256))
selectPlayerById =
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
        from players
        where player_id = $1::int8 and expires_at > now()
      |]

parseSessionCookie :: ByteString -> Either String (PlayerId, ByteString)
parseSessionCookie cookie = case BS.split '.' cookie of
  [idBase64, secretBase64] -> do
    playerId <- Id.decodeByteString idBase64
    secret <- Base64.decode secretBase64
    pure (playerId, secret)
  _ -> Left "invalid session cookie"

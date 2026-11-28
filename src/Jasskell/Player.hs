{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.Vec qualified as Vec
import Hasql.Pipeline qualified
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH (maybeStatement, resultlessStatement)
import Jasskell.Database qualified as DB
import Jasskell.Id (Id)
import Jasskell.Id qualified as Id
import Jasskell.Throw
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteStrict)

type PlayerId = Id Player

newtype Nickname = Nickname {toText :: Text}
  deriving newtype (Eq, Ord, Show, IsString, Vec.Elem)

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
  deriving (Vec.Elem) via Vec.Boxed Player

newSession :: DB.Pool -> Nickname -> IO (Player, SetCookie)
newSession db nickname = do
  playerId <- Id.new
  sessionId <- Id.new
  secret <- liftIO $ getRandomBytes 32
  let !secretHash = hash @ByteString @SHA256 secret
      !player = Player {id = playerId, nickname}
      !session = Session {id = sessionId, secret}
  DB.pipeline db $
    Hasql.Pipeline.statement (playerId, nickname) insertPlayer
      *> Hasql.Pipeline.statement (sessionId, playerId, secretHash) insertSession
  pure (player, makeSessionCookie session)

insertPlayer :: Hasql.Statement (PlayerId, Nickname) ()
insertPlayer =
  lmap
    ( \(playerId, nickname) ->
        (Id.toInt64 playerId, nickname.toText)
    )
    [resultlessStatement|
      insert into players (player_id, nickname)
      values ($1::int8, $2::text)
    |]

insertSession :: Hasql.Statement (SessionId, PlayerId, Digest SHA256) ()
insertSession =
  lmap
    ( \(sessionId, playerId, secretHash) ->
        (Id.toInt64 sessionId, Id.toInt64 playerId, convert secretHash)
    )
    [resultlessStatement|
      insert into sessions (session_id, player_id, secret_sha256)
      values ($1::int8, $2::int8, $3::bytea)
    |]

type SessionId = Id Session

data Session = Session
  { id :: SessionId,
    secret :: ByteString
  }

sessionCookieName :: (IsString s) => s
sessionCookieName = "session"

makeSessionCookie :: Session -> SetCookie
makeSessionCookie session =
  defaultSetCookie
    { setCookieName = sessionCookieName,
      setCookieValue =
        BS.intercalate
          "."
          [ Id.encodeByteString session.id,
            Base64.encode session.secret
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

fromSessionCookie :: DB.Pool -> ByteString -> IO (Either SessionError Player)
fromSessionCookie db cookie = trying $ \sessionError -> do
  session <- case parseSessionCookie cookie of
    Left e -> throw sessionError $ InvalidSessionCookie e
    Right x -> pure x
  (player, secretSHA256) <-
    DB.use db (Hasql.statement session.id selectPlayerBySessionId)
      >>= \case
        Nothing -> throw sessionError $ SessionNotFound session.id
        Just x -> pure x
  unless (hash session.secret == secretSHA256) $
    throw sessionError SessionSecretHashMismatch
  pure player

selectPlayerBySessionId :: Hasql.Statement SessionId (Maybe (Player, Digest SHA256))
selectPlayerBySessionId =
  lmap Id.toInt64 $
    Hasql.refineResult
      ( \case
          Nothing -> Right Nothing
          Just (playerId, nickname, secretHash) -> case digestFromByteString secretHash of
            Nothing -> Left $ "invalid sha256 length: " <> Text.show (BS.length secretHash)
            Just secretSHA256 ->
              let player = Player {id = Id.fromInt64 playerId, nickname = coerce nickname}
               in Right $ Just (player, secretSHA256)
      )
      [maybeStatement|
        select p.player_id::int8, p.nickname::text, s.secret_sha256::bytea
        from sessions s
        join players p on s.player_id = p.player_id
        where session_id = $1::int8 and expires_at > now()
      |]

parseSessionCookie :: ByteString -> Either String Session
parseSessionCookie cookie = case BS.split '.' cookie of
  [idBase64, secretBase64] -> do
    sessionId <- Id.decodeByteString idBase64
    secret <- Base64.decode secretBase64
    pure $ Session {id = sessionId, secret}
  _ -> Left "invalid session cookie"

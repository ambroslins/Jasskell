{-# LANGUAGE ViewPatterns #-}

module Jasskell.Server (application) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Exception (Exception, throwIO)
import Control.Monad (forever, when, (<=<))
import Data.Aeson (eitherDecode, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits (KnownNat)
import Jasskell.Clock (Clock, Duration, Timestamp, diff)
import Jasskell.Clock qualified as Clock
import Jasskell.Database qualified as DB
import Jasskell.Form qualified as Form
import Jasskell.Game (Action, GameId, Manager, Message (..))
import Jasskell.Game qualified as Game
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Player (Player (id))
import Jasskell.Player qualified as Player
import Jasskell.Render qualified as Render
import Jasskell.Static qualified as Static
import Jasskell.Throw
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection qualified as WS
import Network.WebSockets.Connection.PingPong qualified as WS
import System.Random qualified as Random
import Web.Cookie (SetCookie, parseCookies, renderSetCookieBS)

application :: Logger -> Clock -> DB.Pool -> Manager -> Wai.Application
application logger clock db mangaer =
  websocketsOr WS.defaultConnectionOptions (websocketApp logger clock db mangaer)
    . requestLogger logger
    . Static.handlers
    $ routes logger db

websocketApp :: Logger -> Clock -> DB.Pool -> Manager -> WS.ServerApp
websocketApp logger clock db tm pending = handling handleReject $ \reject -> do
  let request = WS.pendingRequest pending
  gameId <- case parseGameId (WS.requestPath request) of
    Nothing -> throw reject $ WS.defaultRejectRequest {WS.rejectCode = 404}
    Just t -> pure t
  sessionCookie <- case parseSessionCookie (WS.requestHeaders request) of
    Nothing -> throw reject $ WS.defaultRejectRequest {WS.rejectCode = 401}
    Just sc -> pure sc
  player <-
    Player.fromSessionCookie db sessionCookie >>= \case
      Left err -> do
        logger.error "invalid session cookie" ["error" =: show err]
        throw reject $ WS.defaultRejectRequest {WS.rejectCode = 401}
      Right s -> pure s
  result <- Game.join logger db player gameId tm $
    \sendAction receiveMsg -> do
      conn <- WS.acceptRequest pending
      lastPong <- Clock.now clock >>= STM.newTVarIO
      let options = WS.connectionOptions conn
          onPong = Clock.now clock >>= STM.atomically . STM.writeTVar lastPong
          connection = conn {WS.connectionOptions = options {WS.connectionOnPong = onPong}}
      runWebSocketConnection logger connection lastPong sendAction receiveMsg
  maybe (throw reject $ WS.defaultRejectRequest {WS.rejectCode = 404}) pure result
  where
    handleReject rr = do
      logger.error "reject websocket request" ["code" =: WS.rejectCode rr]
      WS.rejectRequestWith pending rr
    parseGameId path = do
      t <- BS.stripPrefix "/games/" path
      either (const Nothing) Just $ Id.decodeByteString t
    parseSessionCookie headers = do
      cookies <- lookup HTTP.hCookie headers
      lookup Player.sessionCookieName $ parseCookies cookies

data ReceiverExited = ReceiverExited
  deriving (Eq, Show)

instance Exception ReceiverExited

pingInterval, pongTimeout :: Duration
pingInterval = Clock.seconds 20
pongTimeout = Clock.seconds 60

runWebSocketConnection ::
  (KnownNat n) =>
  Logger ->
  WS.Connection ->
  STM.TVar Timestamp ->
  (Action n -> STM ()) ->
  STM (Message n) ->
  IO ()
runWebSocketConnection logger connection lastPong sendAction receiveMsg = do
  Async.withAsync receiver $ \receiverThread -> fix $ \loop -> do
    let waitReceiver = Async.waitSTM receiverThread >> STM.throwSTM ReceiverExited
    msg <- STM.atomically $ waitReceiver <|> receiveMsg
    case msg of
      GameClosed -> closeWith Render.closedView "closed"
      Superseded -> closeWith Render.supersededView "superseded"
      Rejected rejection -> do
        -- JSON without a "content" field is not swapped by hx-ws;
        -- toast.js picks it up instead.
        WS.sendTextData connection $
          Aeson.encode $
            Aeson.object
              [ "toast" .= Render.rejectionMessage rejection,
                "type" .= ("error" :: Text)
              ]
        loop
      Tickle tick -> do
        pong <- STM.readTVarIO lastPong
        let d = tick `diff` pong
        when (d > pongTimeout) $ throwIO WS.PongTimeout
        when (d > pingInterval) $ WS.sendPing connection (Text.show tick)
        loop
      UpdateWaiting view -> do
        sendBuilder $ Render.fragment $ Render.waitingView view
        loop
      UpdatePlayer view -> do
        sendBuilder $ Render.fragment $ Render.playerView view
        loop
      UpdateSpectator view -> do
        sendBuilder $ Render.fragment $ Render.spectatorView view
        loop
  where
    receiver = forever $ do
      msg <- WS.receiveData @LazyByteString connection
      case eitherDecode msg of
        Left err -> logger.error "decode websocket message" ["error" =: err]
        Right action -> STM.atomically $ sendAction action
    sendBuilder = WS.sendTextData connection . Builder.toLazyByteString
    -- Render a final message and send a normal close frame (code 1000).
    -- Without it the client sees an abnormal close (1006) and hx-ws
    -- reconnects, which would e.g. let two tabs supersede each other forever.
    closeWith view reason = do
      sendBuilder $ Render.fragment view
      WS.sendClose connection (reason :: Text)

routes :: Logger -> DB.Pool -> Wai.Application
routes logger db request respond =
  respond =<< case Wai.pathInfo request of
    [] | method == HTTP.methodGet -> getRoot logger db request
    ["games"]
      | method == HTTP.methodGet -> getGames db request
      | method == HTTP.methodPost -> postGames logger db request
    ["games", Id.decodeText -> Right gameId]
      | method == HTTP.methodGet -> getGame logger db gameId request
    ["games", Id.decodeText -> Right gameId, "join"]
      | method == HTTP.methodPost -> postGameJoin db gameId request
    _ -> pure notFound
  where
    method = Wai.requestMethod request

notFound :: Wai.Response
notFound = Wai.responseLBS HTTP.status404 [] "Not Found"

responseHtml :: HTTP.ResponseHeaders -> Builder -> Wai.Response
responseHtml headers =
  Wai.responseBuilder HTTP.status200 (ct : headers)
  where
    ct = (HTTP.hContentType, "text/html; charset=utf-8")

getRoot :: Logger -> DB.Pool -> Wai.Request -> IO Wai.Response
getRoot logger db request = do
  mplayer <- handling (const $ pure Nothing) $ \sessionError ->
    getPlayerSession logger db sessionError request
  stdGen <- Random.newStdGen
  pure . responseHtml [] . Render.page "Jasskell" $ Render.index stdGen mplayer

getGames :: DB.Pool -> Wai.Request -> IO Wai.Response
getGames db _request = do
  games <- Game.getPublicGames db
  pure . responseHtml [] . Render.fragment $ Render.gameList games

postGames :: Logger -> DB.Pool -> Wai.Request -> IO Wai.Response
postGames logger db request = do
  requestBody <- consumeRequestBody request
  handling (pure . responseSessionError) $ \sessionError -> do
    msession <- getPlayerSession logger db sessionError request
    handling (pure . responseFormErrors) $ \formErrors -> do
      let sessionParser = case msession of
            Nothing ->
              Form.field "nickname" (Player.parseNickname <=< Form.text)
                <&> \nickname -> do
                  (player, setCookie) <- Player.newSession db nickname
                  pure (player.id, [setCookieHeader setCookie])
            Just player -> pure $ pure (player.id, [])
          -- An unchecked checkbox is not submitted at all.
          privateParser = isJust <$> Form.optional "private" (const $ Right ())
          parser = (,) <$> sessionParser <*> privateParser
      (makeSession, private) <- either (throw formErrors) pure $ Form.parseByteString parser requestBody
      (playerId, headers) <- makeSession
      gameId <- Game.create db playerId private
      let location = "/games/" <> Id.encodeByteString gameId
      pure $
        Wai.responseBuilder
          HTTP.status303
          ((HTTP.hLocation, location) : headers)
          mempty
  where
    responseFormErrors = Wai.responseBuilder HTTP.status400 [] . Form.renderErrors
    responseSessionError = const $ Wai.responseLBS HTTP.status401 [] "Invalid Session"

getGame :: Logger -> DB.Pool -> GameId -> Wai.Request -> IO Wai.Response
getGame logger db gameId request = do
  mplayer <- handling (const $ pure Nothing) $
    \sessionError -> getPlayerSession logger db sessionError request
  Game.getGameInfo db gameId >>= \case
    Nothing -> pure notFound
    Just info -> pure . responseHtml [] . Render.page "Jasskell" $ case mplayer of
      Nothing -> Render.gameLogin gameId info
      Just player -> Render.gameConnect player gameId

postGameJoin :: DB.Pool -> GameId -> Wai.Request -> IO Wai.Response
postGameJoin db gameId request = do
  requestBody <- consumeRequestBody request
  case Form.parseByteString formParser requestBody of
    Left errors ->
      pure . Wai.responseBuilder HTTP.status400 [] $
        Form.renderErrors errors
    Right nickname -> do
      (player, setCookie) <- Player.newSession db nickname
      pure
        . responseHtml [setCookieHeader setCookie]
        . Render.fragment
        $ Render.gameConnect player gameId
  where
    formParser = Form.field "nickname" (Player.parseNickname <=< Form.text)

consumeRequestBody :: Wai.Request -> IO ByteString
consumeRequestBody = fmap BS.toStrict . Wai.consumeRequestBodyStrict

getPlayerSession :: Logger -> DB.Pool -> Throw Player.SessionError -> Wai.Request -> IO (Maybe Player)
getPlayerSession logger db sessionError request =
  let mSessionCookie = do
        cookies <- lookup HTTP.hCookie $ Wai.requestHeaders request
        lookup Player.sessionCookieName $ parseCookies cookies
   in case mSessionCookie of
        Nothing -> pure Nothing
        Just sessionCookie ->
          Player.fromSessionCookie db sessionCookie >>= \case
            Left err -> do
              logger.warning "invalid session cookie" ["error" =: show err]
              throw sessionError err
            Right player -> pure $ Just player

setCookieHeader :: SetCookie -> HTTP.Header
setCookieHeader setCookie = ("Set-Cookie", renderSetCookieBS setCookie)

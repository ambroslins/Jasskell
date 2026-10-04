{-# LANGUAGE ViewPatterns #-}

module Jasskell.Server (application) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Monad ((<=<))
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.Text (Text)
import Jasskell.Database qualified as DB
import Jasskell.Form qualified as Form
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Player (Player)
import Jasskell.Player qualified as Player
import Jasskell.Render qualified as Render
import Jasskell.Static qualified as Static
import Jasskell.Table (Connection (..), Message (..), TableId, TableManager)
import Jasskell.Table qualified as Table
import Jasskell.Throw
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import System.Random qualified as Random
import Web.Cookie (SetCookie, parseCookies, renderSetCookieBS)

application :: Logger -> DB.Pool -> TableManager -> Wai.Application
application logger db tableManager =
  websocketsOr WS.defaultConnectionOptions (websocketApp logger db tableManager)
    . requestLogger logger
    . Static.handlers
    $ routes logger db

websocketApp :: Logger -> DB.Pool -> TableManager -> WS.ServerApp
websocketApp logger db tm pending = handling handleReject $ \reject -> do
  let request = WS.pendingRequest pending
  tableId <- case parseTableId (WS.requestPath request) of
    Nothing -> throw reject $ WS.defaultRejectRequest {WS.rejectCode = 404}
    Just t -> pure t
  sessionCookie <- case parseSessionCookie (WS.requestHeaders request) of
    Nothing -> throw reject $ WS.defaultRejectRequest {WS.rejectCode = 401}
    Just sc -> pure sc
  player <-
    Player.fromSessionCookie db sessionCookie >>= \case
      Left err -> do
        logError logger "invalid session cookie" ["error" =: show err]
        throw reject $ WS.defaultRejectRequest {WS.rejectCode = 401}
      Right s -> pure s
  Table.join logger db player tableId tm $ \case
    Nothing -> throw reject $ WS.defaultRejectRequest {WS.rejectCode = 404}
    Just tableConn -> do
      wsConn <- WS.acceptRequest pending
      WS.withPingThread wsConn 30 (pure ()) $ do
        logDebug logger "accepted websocket request" []
        let sendLoop = do
              msg <- WS.receiveData @LazyByteString wsConn
              case eitherDecode msg of
                Left err -> logError logger "decode websocket message" ["error" =: err]
                Right cmd -> do
                  logDebug logger "got command" ["message" =: show cmd]
                  STM.atomically $ tableConn.send cmd
              sendLoop
            receiveLoop = do
              msg <- STM.atomically tableConn.receive
              logDebug logger "got table message" ["message" =: show msg]
              case msg of
                ConnectionClosed -> WS.sendTextData @Text wsConn "closed"
                UpdateWaiting view -> do
                  sendBuilder wsConn $ Render.fragment $ Render.waitingView view
                  receiveLoop
                UpdatePlayer view -> do
                  sendBuilder wsConn $ Render.fragment $ Render.playerView view
                  receiveLoop
                UpdateSpectator view -> do
                  sendBuilder wsConn $ Render.fragment $ Render.spectatorView view
                  receiveLoop
        Async.race_ sendLoop receiveLoop
  where
    handleReject rr = do
      logError logger "reject websocket request" ["code" =: WS.rejectCode rr]
      WS.rejectRequestWith pending rr
    parseTableId path = do
      t <- BS.stripPrefix "/tables/" path
      either (const Nothing) Just $ Id.decodeByteString t
    parseSessionCookie headers = do
      cookies <- lookup HTTP.hCookie headers
      lookup Player.sessionCookieName $ parseCookies cookies
    sendBuilder c = WS.sendTextData c . Builder.toLazyByteString

routes :: Logger -> DB.Pool -> Wai.Application
routes logger db request respond =
  respond =<< case Wai.pathInfo request of
    [] | method == HTTP.methodGet -> getRoot logger db request
    ["tables"]
      | method == HTTP.methodGet -> getTables db request
      | method == HTTP.methodPost -> postTables logger db request
    ["tables", Id.decodeText -> Right tableId]
      | method == HTTP.methodGet -> getTable logger db tableId request
    ["tables", Id.decodeText -> Right tableId, "join"]
      | method == HTTP.methodPost -> postTableJoin db tableId request
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
  mplayer <- getPlayerSession logger db request
  stdGen <- Random.newStdGen
  pure . responseHtml [] . Render.page "Jasskell" $ Render.index stdGen mplayer

getTables :: DB.Pool -> Wai.Request -> IO Wai.Response
getTables db _request = do
  tables <- Table.getPublicTables db
  pure . responseHtml [] . Render.fragment $ Render.tableList tables

postTables :: Logger -> DB.Pool -> Wai.Request -> IO Wai.Response
postTables logger db request = do
  requestBody <- consumeRequestBody request
  case Form.parseByteString formParser requestBody of
    Left errors ->
      pure . Wai.responseBuilder HTTP.status400 [] $
        Form.renderErrors errors
    Right mnickname ->
      getPlayerSession logger db request >>= \case
        Nothing -> case mnickname of
          Nothing ->
            pure $ Wai.responseBuilder HTTP.status401 [] "Unauthorized"
          Just nickname -> do
            (player, setCookie) <- Player.newSession db nickname
            createTable player.id [setCookieHeader setCookie]
        Just player -> createTable player.id []
  where
    formParser = Form.optional "nickname" (Player.parseNickname <=< Form.text)
    createTable playerId headers = do
      tableId <- Table.create db playerId
      let location = "/tables/" <> Id.encodeByteString tableId
      pure $
        Wai.responseBuilder
          HTTP.status303
          ((HTTP.hLocation, location) : headers)
          mempty

getTable :: Logger -> DB.Pool -> TableId -> Wai.Request -> IO Wai.Response
getTable logger db tableId request = do
  mplayer <- getPlayerSession logger db request
  pure . responseHtml [] . Render.page "Jaskell" $ case mplayer of
    Nothing -> Render.tableLogin tableId
    Just player -> Render.tableConnect player tableId

postTableJoin :: DB.Pool -> TableId -> Wai.Request -> IO Wai.Response
postTableJoin db tableId request = do
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
        $ Render.tableConnect player tableId
  where
    formParser = Form.field "nickname" (Player.parseNickname <=< Form.text)

consumeRequestBody :: Wai.Request -> IO ByteString
consumeRequestBody = fmap BS.toStrict . Wai.consumeRequestBodyStrict

getPlayerSession :: Logger -> DB.Pool -> Wai.Request -> IO (Maybe Player)
getPlayerSession logger db request =
  let mSessionCookie = do
        cookies <- lookup HTTP.hCookie $ Wai.requestHeaders request
        lookup Player.sessionCookieName $ parseCookies cookies
   in case mSessionCookie of
        Nothing -> pure Nothing
        Just sessionCookie ->
          Player.fromSessionCookie db sessionCookie >>= \case
            Left err -> do
              logWarning logger "invalid session cookie" ["error" =: show err]
              pure Nothing
            Right player -> pure $ Just player

setCookieHeader :: SetCookie -> HTTP.Header
setCookieHeader setCookie = ("Set-Cookie", renderSetCookieBS setCookie)

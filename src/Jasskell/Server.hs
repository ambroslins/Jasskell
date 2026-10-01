{-# LANGUAGE ViewPatterns #-}

module Jasskell.Server (application) where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (liftIOOp)
import Control.Monad.Trans (lift)
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.Text (Text)
import Jasskell.App (AppT, Env (..), hoistAppT, runAppT)
import Jasskell.Form qualified as Form
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Player (Player)
import Jasskell.Player qualified as Player
import Jasskell.Render qualified as Render
import Jasskell.Static qualified as Static
import Jasskell.Table (Connection (..), Message (..), TableId, TableManager)
import Jasskell.Table qualified as Table
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import System.Random qualified as Random
import UnliftIO.Async qualified as Async
import UnliftIO.STM (atomically)
import Web.Cookie (SetCookie, parseCookies, renderSetCookieBS)

application :: Env -> TableManager -> Wai.Application
application env tableManager =
  websocketsOr WS.defaultConnectionOptions (websocketApp env tableManager)
    . requestLogger env.logger
    . Static.handlers
    $ routes env tableManager

websocketApp :: Env -> TableManager -> WS.ServerApp
websocketApp env tm pending = rejectOnError . runExceptT . runAppT env $ do
  let request = WS.pendingRequest pending
  tableId <- case parseTableId (WS.requestPath request) of
    Nothing -> throwError $ WS.defaultRejectRequest {WS.rejectCode = 404}
    Just t -> pure t
  sessionCookie <- case parseSessionCookie (WS.requestHeaders request) of
    Nothing -> throwError $ WS.defaultRejectRequest {WS.rejectCode = 401}
    Just sc -> pure sc
  player <-
    Player.fromSessionCookie sessionCookie >>= \case
      Left err -> do
        logError "invalid session cookie" ["error" =: show err]
        throwError $ WS.defaultRejectRequest {WS.rejectCode = 401}
      Right s -> pure s
  hoistAppT ExceptT $
    Table.join player tableId tm $
      runExceptT . \case
        Nothing -> throwError $ WS.defaultRejectRequest {WS.rejectCode = 404}
        Just tableConn -> lift $ do
          wsConn <- liftIO $ WS.acceptRequest pending
          liftIOOp (WS.withPingThread wsConn 30 (pure ())) $ do
            logDebug "accepted websocket request" []
            let sendLoop = do
                  msg <- liftIO $ WS.receiveData @LazyByteString wsConn
                  case eitherDecode msg of
                    Left err -> logError "decode websocket message" ["error" =: err]
                    Right cmd -> do
                      logDebug "got command" ["message" =: show cmd]
                      atomically $ tableConn.send cmd
                  sendLoop
                receiveLoop = do
                  msg <- atomically tableConn.receive
                  logDebug "got table message" ["message" =: show msg]
                  case msg of
                    ConnectionClosed -> liftIO $ WS.sendTextData @Text wsConn "closed"
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
    rejectOnError m =
      m >>= \case
        Left rr -> runAppT env $ do
          logError "reject websocket request" ["code" =: WS.rejectCode rr]
          liftIO $ WS.rejectRequestWith pending rr
        Right () -> pure ()
    parseTableId path = do
      t <- BS.stripPrefix "/tables/" path
      either (const Nothing) Just $ Id.decodeByteString t
    parseSessionCookie headers = do
      cookies <- lookup HTTP.hCookie headers
      lookup Player.sessionCookieName $ parseCookies cookies
    sendBuilder c = liftIO . WS.sendTextData c . Builder.toLazyByteString

routes :: Env -> TableManager -> Wai.Application
routes env tm request respond = respond <=< runAppT env $
  case Wai.pathInfo request of
    [] | method == HTTP.methodGet -> getRoot request
    ["tables"]
      | method == HTTP.methodGet -> getTables request
      | method == HTTP.methodPost -> postTables request
    ["tables", Id.decodeText -> Right tableId]
      | method == HTTP.methodGet -> getTable tableId request
    ["tables", Id.decodeText -> Right tableId, "join"]
      | method == HTTP.methodPost -> postTableJoin tableId request
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

getRoot :: Wai.Request -> AppT IO Wai.Response
getRoot request = do
  mplayer <- getPlayerSession request
  stdGen <- Random.newStdGen
  pure . responseHtml [] . Render.page "Jasskell" $ Render.index stdGen mplayer

getTables :: Wai.Request -> AppT IO Wai.Response
getTables _request = do
  tables <- Table.getPublicTables
  pure . responseHtml [] . Render.fragment $ Render.tableList tables

postTables :: Wai.Request -> AppT IO Wai.Response
postTables request = do
  requestBody <- consumeRequestBody request
  case Form.parseByteString formParser requestBody of
    Left errors ->
      pure . Wai.responseBuilder HTTP.status400 [] $
        Form.renderErrors errors
    Right mnickname ->
      getPlayerSession request >>= \case
        Nothing -> case mnickname of
          Nothing ->
            pure $ Wai.responseBuilder HTTP.status401 [] "Unauthorized"
          Just nickname -> do
            (player, setCookie) <- Player.newSession nickname
            createTable player.id [setCookieHeader setCookie]
        Just player -> createTable player.id []
  where
    formParser = Form.optional "nickname" (Player.parseNickname <=< Form.text)
    createTable playerId headers = do
      tableId <- Table.create playerId
      let location = "/tables/" <> Id.encodeByteString tableId
      pure $
        Wai.responseBuilder
          HTTP.status303
          ((HTTP.hLocation, location) : headers)
          mempty

getTable :: TableId -> Wai.Request -> AppT IO Wai.Response
getTable tableId request = do
  mplayer <- getPlayerSession request
  pure . responseHtml [] . Render.page "Jaskell" $ case mplayer of
    Nothing -> Render.tableLogin tableId
    Just player -> Render.tableConnect player tableId

postTableJoin :: TableId -> Wai.Request -> AppT IO Wai.Response
postTableJoin tableId request = do
  requestBody <- consumeRequestBody request
  case Form.parseByteString formParser requestBody of
    Left errors ->
      pure . Wai.responseBuilder HTTP.status400 [] $
        Form.renderErrors errors
    Right nickname -> do
      (player, setCookie) <- Player.newSession nickname
      pure
        . responseHtml [setCookieHeader setCookie]
        . Render.fragment
        $ Render.tableConnect player tableId
  where
    formParser = Form.field "nickname" (Player.parseNickname <=< Form.text)

consumeRequestBody :: (MonadIO m) => Wai.Request -> m ByteString
consumeRequestBody = liftIO . fmap BS.toStrict . Wai.consumeRequestBodyStrict

getPlayerSession :: Wai.Request -> AppT IO (Maybe Player)
getPlayerSession request =
  let mSessionCookie = do
        cookies <- lookup HTTP.hCookie $ Wai.requestHeaders request
        lookup Player.sessionCookieName $ parseCookies cookies
   in case mSessionCookie of
        Nothing -> pure Nothing
        Just sessionCookie ->
          Player.fromSessionCookie sessionCookie >>= \case
            Left err -> do
              logWarning "invalid session cookie" ["error" =: show err]
              pure Nothing
            Right player -> pure $ Just player

setCookieHeader :: SetCookie -> HTTP.Header
setCookieHeader setCookie = ("Set-Cookie", renderSetCookieBS setCookie)

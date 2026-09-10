module Jasskell.Server (application) where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (liftIOOp)
import Control.Monad.Trans (lift)
import Data.Aeson (eitherDecode)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.Text (Text)
import Jasskell.App (AppT, Env (..), hoistAppT, runAppT)
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Player (Player)
import Jasskell.Player qualified as Player
import Jasskell.Render qualified as Render
import Jasskell.Static qualified as Static
import Jasskell.Table (Connection (..), Message (..), TableId, TableManager)
import Jasskell.Table qualified as Table
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import UnliftIO.Async qualified as Async
import UnliftIO.STM (atomically)
import Web.Cookie (SetCookie, parseCookies, renderSetCookieBS)
import Web.Twain qualified as Twain

application :: Env -> TableManager -> Wai.Application
application env tableManager =
  websocketsOr
    WS.defaultConnectionOptions
    (websocketApp env tableManager)
    $ requestLogger env.logger
    $ foldr ($) (Twain.notFound $ Twain.send $ Twain.html "Not found...")
    $ Static.handlers
      : routes env tableManager

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
      cookies <- lookup Twain.hCookie headers
      lookup Player.sessionCookieName $ parseCookies cookies
    sendBuilder c = liftIO . WS.sendTextData c . Builder.toLazyByteString

routes :: Env -> TableManager -> [Twain.Middleware]
routes env tm =
  [ Twain.get "/" $ runAppT env $ getRoot tm,
    Twain.post "/tables" $ runAppT env postTables,
    Twain.get "/tables/:table-id" $ runAppT env getTable,
    Twain.post "/tables/:table-id/join" $ runAppT env postTableJoin
  ]

sendHtml :: Twain.ResponseHeaders -> Builder -> AppT Twain.ResponderM ()
sendHtml headers =
  lift . Twain.send . Wai.responseBuilder Twain.status200 (ct : headers)
  where
    ct = (Twain.hContentType, "text/html; charset=utf-8")

getRoot :: TableManager -> AppT Twain.ResponderM ()
getRoot _tm = do
  mplayer <- getPlayerSession
  sendHtml [] . Render.page "Jasskell" $ Render.index mplayer

postTables :: AppT Twain.ResponderM ()
postTables = do
  _private <- lift $ Twain.paramMaybe @Bool "private"
  (playerId, addCookie) <-
    getPlayerSession >>= \case
      Just p -> pure (p.id, id)
      Nothing -> do
        nickname <- lift $ Twain.param "nickname"
        (p, setCookie) <- Player.newSession nickname
        pure (p.id, Twain.withCookie' setCookie)
  tableId <- Table.create playerId
  lift $ Twain.send $ addCookie . Twain.redirect303 $ "/tables/" <> Id.encodeText tableId

getTable :: AppT Twain.ResponderM ()
getTable = do
  tableId <- lift $ Twain.param @TableId "table-id"
  mplayer <- getPlayerSession
  sendHtml [] . Render.page "Jaskell" $ case mplayer of
    Nothing -> Render.tableLogin tableId
    Just player -> Render.tableConnect player tableId

postTableJoin :: AppT Twain.ResponderM ()
postTableJoin = do
  tableId <- lift $ Twain.param @TableId "table-id"
  nickname <- lift $ Twain.param "nickname"
  mplayer <- getPlayerSession
  case mplayer of
    Just _ -> error "TODO: change nickname"
    Nothing -> do
      (player, setCookie) <- Player.newSession nickname
      sendHtml [setCookieHeader setCookie] $
        Render.fragment $
          Render.tableConnect player tableId

getPlayerSession :: AppT Twain.ResponderM (Maybe Player)
getPlayerSession =
  lift (Twain.cookieParamMaybe Player.sessionCookieName) >>= \case
    Nothing -> pure Nothing
    Just sessionCookie ->
      Player.fromSessionCookie sessionCookie >>= \case
        Left err -> do
          logWarning "invalid session cookie" ["error" =: show err]
          lift . Twain.send . Twain.status Twain.status401 . Twain.expireCookie Player.sessionCookieName $
            Twain.text "Unauthorized"
        Right player -> pure $ Just player

setCookieHeader :: SetCookie -> Twain.Header
setCookieHeader setCookie = ("Set-Cookie", renderSetCookieBS setCookie)

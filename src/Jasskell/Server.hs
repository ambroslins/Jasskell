module Jasskell.Server (application) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as Text
import Jasskell.App (AppT, Env (..), runAppT)
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Session (Session)
import Jasskell.Session qualified as Session
import Jasskell.Skeleton (skeleton)
import Jasskell.Static qualified as Static
import Jasskell.Table (Table (..), TableId, TableManager)
import Jasskell.Table qualified as Table
import Lucid
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_, hxWsConnect_)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Web.Cookie (parseCookies)
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
websocketApp env tm pending = runAppT env . rejectOnError . runExceptT $ do
  let request = WS.pendingRequest pending
  tableId <- case parseTableId (WS.requestPath request) of
    Nothing -> throwError $ WS.defaultRejectRequest {WS.rejectCode = 404}
    Just t -> pure t
  sessionCookie <- case parseSessionCookie (WS.requestHeaders request) of
    Nothing -> throwError $ WS.defaultRejectRequest {WS.rejectCode = 401}
    Just sc -> pure sc
  session <-
    lift (Session.get sessionCookie) >>= \case
      Left err -> do
        logError "invalid session cookie" ["error" =: show err]
        throwError $ WS.defaultRejectRequest {WS.rejectCode = 401}
      Right s -> pure s
  table <-
    liftIO (Table.lookup tableId tm) >>= \case
      Nothing -> throwError $ WS.defaultRejectRequest {WS.rejectCode = 404}
      Just t -> pure t
  connection <- liftIO $ WS.acceptRequest pending
  logDebug "got websocket connection" ["session-id" =: Id.encodeText session.id]
  liftIO $ Table.withClient table session $ \_send receive -> do
    let sendLoop = do
          msg <- WS.receiveData connection
          putStrLn $ "got message: " <> Text.unpack msg
          sendLoop
        receiveLoop = do
          msg <- STM.atomically receive
          WS.sendTextData connection $ Text.show msg
          receiveLoop
     in Async.race_ sendLoop receiveLoop
  where
    rejectOnError m =
      m >>= \case
        Left rr -> do
          logError "reject websocket request" ["code" =: WS.rejectCode rr]
          liftIO $ WS.rejectRequestWith pending rr
        Right () -> pure ()
    parseTableId path = do
      t <- BS.stripPrefix "/tables/" path
      either (const Nothing) Just $ Id.decodeByteString t
    parseSessionCookie headers = do
      cookies <- lookup Twain.hCookie headers
      lookup Session.cookieName $ parseCookies cookies

routes :: Env -> TableManager -> [Twain.Middleware]
routes env tm =
  [ Twain.get "/" $ runAppT env $ getRoot tm,
    Twain.post "/tables" $ runAppT env $ postTables tm,
    Twain.get "/tables/:table-id" $ runAppT env $ getTable tm,
    Twain.post "/tables/:table-id/join" $ runAppT env $ postTableJoin tm
  ]

getRoot :: TableManager -> AppT Twain.ResponderM ()
getRoot _tm = do
  lift $ Twain.send $ Twain.html $ renderBS $ skeleton "Jasskell" $ do
    header_ [id_ "top", class_ "container nav"] $ do
      a_ [href_ "#top"] "Jass"
      div_ $
        button_ [class_ "secondary small"] "Sign in"
    main_ $ do
      section_ [id_ "hero", class_ "container"] $ do
        h1_ "Hero Section"
      section_ [id_ "tables", class_ "container"] $ do
        h2_ "Tables"
      section_ [id_ "create", class_ "container"] $ do
        h2_ "Create"
        form_ [method_ "post", action_ "/tables"] $ do
          input_ [type_ "checkbox", name_ "private"]
          button_ [type_ "submit", class_ "primary"] "Create"

postTables :: TableManager -> AppT Twain.ResponderM ()
postTables tm = lift $ do
  _public <- Twain.paramMaybe @Bool "public"
  tableId <- liftIO $ Table.new tm
  Twain.send $ Twain.redirect303 $ "/tables/" <> Id.encodeText tableId

getTable :: TableManager -> AppT Twain.ResponderM ()
getTable tm = do
  tableId <- lift $ Twain.param @TableId "table-id"
  liftIO (Table.lookup tableId tm) >>= \case
    Nothing -> lift . Twain.send . Twain.status Twain.notFound404 $ Twain.text "table not found"
    Just table -> do
      msession <- getSession
      lift . Twain.send . Twain.html . renderBS . skeleton "Jass Table" $ do
        h1_ $ toHtml $ "Found table: " <> Id.encodeText table.id
        main_ $
          case msession of
            Nothing -> form_
              [ hxPost_ $ "/tables/" <> Id.encodeText table.id <> "/join",
                hxTarget_ "main"
              ]
              $ do
                label_ [] $ do
                  "Nickname"
                  input_ [name_ "nickname"]
                button_ [type_ "submit"] "Submit"
            Just session -> viewTable session table.id

viewTable :: Session.Session -> TableId -> Html ()
viewTable session tableId = do
  h2_ $ toHtml $ "Hello: " <> session.nickname.toText
  div_ [hxWsConnect_ $ "/tables/" <> Id.encodeText tableId, hxSwap_ "innerHTML", hxTarget_ "this"] $
    p_ "connecting"

postTableJoin :: TableManager -> AppT Twain.ResponderM ()
postTableJoin _tm = do
  tableId <- lift $ Twain.param @TableId "table-id"
  nickname <- lift $ Twain.param "nickname"
  msession <- getSession
  case msession of
    Just _ -> error "TODO: change nickname"
    Nothing -> do
      (session, setCookie) <- Session.new nickname
      lift . Twain.send . Twain.withCookie' setCookie . Twain.html . renderBS $
        viewTable session tableId

getSession :: AppT Twain.ResponderM (Maybe Session)
getSession =
  lift (Twain.cookieParamMaybe Session.cookieName) >>= \case
    Nothing -> pure Nothing
    Just sessionCookie ->
      Session.get sessionCookie >>= \case
        Left err -> do
          logWarning "invalid session cookie" ["error" =: show err]
          lift . Twain.send . Twain.status Twain.status401 . Twain.expireCookie Session.cookieName $
            Twain.text "Unauthorized"
        Right session -> pure $ Just session

module Jasskell.Server
  ( ServerConfig (..),
    run,
  )
where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans (lift)
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as Text
import Jasskell.Id qualified as Id
import Jasskell.Logger (Logger, logDebug, requestLogger, (=:))
import Jasskell.Session (SessionRegistry)
import Jasskell.Session qualified as Session
import Jasskell.Skeleton (skeleton)
import Jasskell.Static qualified as Static
import Jasskell.Table (Table (..), TableId, TableManager)
import Jasskell.Table qualified as Table
import Jasskell.User (User (..))
import Lucid
import Lucid.Htmx (hxSwap_, hxTarget_, hxWsConnect_)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Web.Twain qualified as Twain

newtype ServerConfig = ServerConfig {port :: Int}

run :: ServerConfig -> Logger -> IO ()
run config logger = do
  sessionRegistry <- Session.newRegistry
  Table.withManager $ \tm ->
    Warp.run config.port $
      websocketsOr WS.defaultConnectionOptions (websocketApp tm) $
        requestLogger logger $
          foldr
            ($)
            (Twain.notFound $ Twain.send $ Twain.html "Not found...")
            ( Static.handlers
                : routes tm sessionRegistry logger
            )

websocketApp :: TableManager -> WS.ServerApp
websocketApp tm pending = do
  let request = WS.pendingRequest pending
  case BS.stripPrefix "/tables/" (WS.requestPath request) of
    Nothing ->
      WS.rejectRequestWith pending $
        WS.defaultRejectRequest {WS.rejectCode = 404}
    Just t -> case Id.decodeByteString t of
      Left _ ->
        WS.rejectRequestWith pending $
          WS.defaultRejectRequest {WS.rejectCode = 400}
      Right tableId ->
        Table.lookup tableId tm >>= \case
          Nothing ->
            WS.rejectRequestWith pending $
              WS.defaultRejectRequest {WS.rejectCode = 404}
          Just table -> do
            connection <- WS.acceptRequest pending
            userId <- Id.new
            let user = User {id = userId, name = "TODO"}
            Table.withClient table user $ \_send receive ->
              let sendLoop = do
                    msg <- WS.receiveData connection
                    putStrLn $ "got message: " <> Text.unpack msg
                    sendLoop
                  receiveLoop = do
                    msg <- STM.atomically receive
                    WS.sendTextData connection $ Text.show msg
                    receiveLoop
               in Async.race_ sendLoop receiveLoop

routes :: TableManager -> SessionRegistry -> Logger -> [Twain.Middleware]
routes tm sr logger =
  [ Twain.get "/" $ runReaderT (getRoot tm sr) logger,
    Twain.post "/tables" $ postTables tm,
    Twain.get "/tables/:table-id" $ getTable tm
  ]

getRoot :: TableManager -> SessionRegistry -> ReaderT Logger Twain.ResponderM ()
getRoot _tm sr = do
  msession <- lift $ Session.getSession sr
  logDebug "get root" ["session" =: show msession]
  setCookie <- case msession of
    Nothing -> do
      (_, sc) <- Session.newSession sr "test"
      pure $ Twain.withCookie' sc
    Just _ -> pure id
  lift $ Twain.send $ setCookie $ Twain.html $ renderBS $ skeleton "Jasskell" $ do
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

postTables :: TableManager -> Twain.ResponderM ()
postTables tm = do
  _public <- Twain.paramMaybe @Bool "public"
  tableId <- liftIO $ Table.new tm
  Twain.send $ Twain.redirect303 $ "/tables/" <> Id.encodeText tableId

getTable :: TableManager -> Twain.ResponderM ()
getTable tm = do
  tableId <- Twain.param @TableId "table-id"
  liftIO (Table.lookup tableId tm) >>= \case
    Nothing -> Twain.send $ Twain.status Twain.notFound404 $ Twain.text "table not found"
    Just table -> Twain.send $ Twain.html $ renderBS $ skeleton "Jass Table" $ do
      h1_ $ toHtml $ "Found table: " <> Id.encodeText table.id
      div_ [hxWsConnect_ $ "/tables/" <> Id.encodeText table.id, hxSwap_ "innerHTML", hxTarget_ "this"] $
        p_ "connecting"

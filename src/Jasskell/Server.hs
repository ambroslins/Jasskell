module Jasskell.Server (application) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as Text
import Jasskell.App (AppT, Env (..), runAppT)
import Jasskell.Id qualified as Id
import Jasskell.Logger (requestLogger)
import Jasskell.Session qualified as Session
import Jasskell.Skeleton (skeleton)
import Jasskell.Static qualified as Static
import Jasskell.Table (Table (..), TableId, TableManager)
import Jasskell.Table qualified as Table
import Jasskell.User (User (..))
import Lucid
import Lucid.Htmx (hxSwap_, hxTarget_, hxWsConnect_)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Web.Twain qualified as Twain

application :: Env -> Session.Registry -> TableManager -> Wai.Application
application env sessionRegistry tableManager =
  websocketsOr WS.defaultConnectionOptions (websocketApp tableManager) $
    requestLogger env.logger $
      foldr ($) (Twain.notFound $ Twain.send $ Twain.html "Not found...") $
        Static.handlers
          : routes env tableManager sessionRegistry

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

routes :: Env -> TableManager -> Session.Registry -> [Twain.Middleware]
routes env tm sr =
  [ Twain.get "/" $ runAppT env $ getRoot tm sr,
    Twain.post "/tables" $ runAppT env $ postTables tm,
    Twain.get "/tables/:table-id" $ runAppT env $ getTable tm
  ]

getRoot :: TableManager -> Session.Registry -> AppT Twain.ResponderM ()
getRoot _tm _sr = do
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
getTable tm = lift $ do
  tableId <- Twain.param @TableId "table-id"
  liftIO (Table.lookup tableId tm) >>= \case
    Nothing -> Twain.send $ Twain.status Twain.notFound404 $ Twain.text "table not found"
    Just table -> Twain.send $ Twain.html $ renderBS $ skeleton "Jass Table" $ do
      h1_ $ toHtml $ "Found table: " <> Id.encodeText table.id
      div_ [hxWsConnect_ $ "/tables/" <> Id.encodeText table.id, hxSwap_ "innerHTML", hxTarget_ "this"] $
        p_ "connecting"

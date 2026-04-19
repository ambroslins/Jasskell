module Jasskell.Server
  ( ServerConfig (..),
    run,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Jasskell.Id qualified as Id
import Jasskell.Skeleton (skeleton)
import Jasskell.Static qualified as Static
import Jasskell.Table (Table (..), TableId, TableManager)
import Jasskell.Table qualified as Table
import Lucid
import Lucid.Htmx (hxSwap_, hxTarget_, hxWsConnect_)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Web.Twain qualified as Twain

newtype ServerConfig = ServerConfig {port :: Int}

run :: ServerConfig -> IO ()
run config =
  Table.withManager $ \tableManager ->
    Warp.run config.port $
      foldr
        ($)
        (Twain.notFound $ Twain.send $ Twain.html "Not found...")
        ( websocketsOr WS.defaultConnectionOptions websocketApp
            : Static.handlers
            : routes tableManager
        )

websocketApp :: WS.ServerApp
websocketApp pending = do
  connection <- WS.acceptRequest pending
  forM_ [1 ..] $ \i -> do
    WS.sendTextData connection ("<p>" <> Text.show @Int i <> "</p>")
    threadDelay 1_000_000

routes :: TableManager -> [Twain.Middleware]
routes tm =
  [ Twain.get "/" $ getRoot tm,
    Twain.post "/tables" $ postTables tm,
    Twain.get "/tables/:table-id" $ getTable tm
  ]

getRoot :: TableManager -> Twain.ResponderM ()
getRoot tm =
  Twain.send $ Twain.html $ renderBS $ skeleton "Jasskell" $ do
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

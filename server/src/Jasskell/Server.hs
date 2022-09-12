module Jasskell.Server where

import Jasskell.Server.API
  ( API,
    NamedAPI (..),
    TableRouts (..),
  )
import Jasskell.Server.App (AppT, runAppT)
import Jasskell.Server.App qualified as App
import Jasskell.Server.Page qualified as Page
import Jasskell.Server.WebSocket qualified as WebSocket
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Servant
  ( Application,
    Handler,
    Raw,
    ServerT,
    hoistServer,
    serve,
    serveDirectoryWebApp,
    type (:<|>) (..),
  )

type Route = API :<|> Page.Route :<|> Raw

route :: Proxy Route
route = Proxy

app :: App.Env IO -> Application
app env =
  websocketsOr
    defaultConnectionOptions
    (WebSocket.server env)
    ( serve route $
        hoistServer
          route
          (runAppT (App.hoistEnv liftIO env))
          server
    )

server :: ServerT Route (AppT Handler)
server =
  ( NamedAPI
      { tableRouts =
          TableRouts
            { postTable = App.createTable
            }
      }
  )
    :<|> Page.server
    :<|> serveDirectoryWebApp "./client/static/"

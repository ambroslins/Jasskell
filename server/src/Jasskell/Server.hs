module Jasskell.Server where

import Jasskell.Server.API
  ( API,
    NamedAPI (..),
    TableRouts (..),
  )
import Jasskell.Server.Env (Env)
import Jasskell.Server.Env qualified as Env
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

app :: Env -> Application
app env =
  websocketsOr
    defaultConnectionOptions
    (WebSocket.server env)
    (serve route $ hoistServer route (`runReaderT` env) server)

server :: ServerT Route (ReaderT Env Handler)
server =
  ( NamedAPI
      { tableRouts =
          TableRouts
            { postTable = Env.createTable
            }
      }
  )
    :<|> Page.server
    :<|> serveDirectoryWebApp "./client/static/"

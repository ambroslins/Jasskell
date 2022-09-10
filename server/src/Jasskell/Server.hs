module Jasskell.Server where

import Jasskell.Server.API
  ( API,
    NamedAPI (..),
    TableRouts (..),
  )
import Jasskell.Server.Page qualified as Page
import Jasskell.Server.ServerState (ServerState)
import Jasskell.Server.ServerState qualified as ServerState
import Jasskell.Server.WebSocket qualified as WebSocket
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Servant (Application, Raw, Server, serve, serveDirectoryWebApp, type (:<|>) (..))

type Route = API :<|> Page.Route :<|> Raw

app :: ServerState -> Application
app serverState =
  websocketsOr
    defaultConnectionOptions
    (WebSocket.server serverState)
    (serve (Proxy @Route) (server serverState))

server :: ServerState -> Server Route
server serverState =
  ( NamedAPI
      { tableRouts =
          TableRouts
            { postTable = liftIO $ ServerState.createTable serverState
            }
      }
  )
    :<|> Page.server
    :<|> serveDirectoryWebApp "./client/static/"

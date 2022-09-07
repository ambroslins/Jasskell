module Jasskell.Server where

import Jasskell.Server.API
  ( API,
    NamedAPI (..),
    TableRouts (..),
  )
import Jasskell.Server.Html qualified as Html
import Jasskell.Server.ServerState (ServerState)
import Jasskell.Server.ServerState qualified as ServerState
import Jasskell.Server.WebSocket qualified as WebSocket
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Servant (Application, Raw, Server, serve, serveDirectoryWebApp, type (:<|>) (..))

type Route = API :<|> Html.Route :<|> Raw

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
    :<|> Html.server
    :<|> serveDirectoryWebApp "./client/static/"

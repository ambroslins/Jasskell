module Jasskell.Server where

import Jasskell.Server.API
  ( API,
    NamedAPI (..),
    TableRouts (..),
  )
import Jasskell.Server.Html qualified as Html
import Jasskell.Server.ServerState (ServerState)
import Jasskell.Server.ServerState qualified as ServerState
import Servant (Application, Server, serve, type (:<|>) (..))

type Route = API :<|> Html.Route

app :: ServerState -> Application
app serverState = serve (Proxy @Route) (server serverState)

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
module Jasskell.Server where

import Jasskell.Server.API
  ( API,
    NamedAPI (..),
    TableRouts (..),
    api,
  )
import Jasskell.Server.ServerState (ServerState)
import Jasskell.Server.ServerState qualified as ServerState
import Servant (Application, Server, serve)

app :: ServerState -> Application
app serverState = serve api (server serverState)

server :: ServerState -> Server API
server serverState =
  NamedAPI
    { tableRouts =
        TableRouts
          { postTable = liftIO $ ServerState.createTable serverState
          }
    }

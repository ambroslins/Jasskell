module Jasskell.Server where

import Jasskell.Server.API qualified as API
import Jasskell.Server.App qualified as App
import Jasskell.Server.Http qualified as Http
import Jasskell.Server.Page qualified as Page
import Jasskell.Server.WebSocket qualified as WebSocket
import Network.Wai (Application)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions)

app :: App.Env IO -> Application
app env =
  websocketsOr
    defaultConnectionOptions
    (WebSocket.server env)
    (Http.serve env static (API.routes <> Page.routes))

static :: Application
static = staticApp $ defaultWebAppSettings "./client/static/"

module Jasskell.Server where

import Jasskell.Server.API qualified as API
import Jasskell.Server.App (MonadApp)
import Jasskell.Server.Http qualified as Http
import Jasskell.Server.Page qualified as Page
import Jasskell.Server.WebSocket qualified as WebSocket
import Network.Wai (Application)
import Network.Wai.Application.Static
  ( StaticSettings (ssMaxAge),
    defaultWebAppSettings,
    staticApp,
  )
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions)
import UnliftIO (MonadUnliftIO)
import WaiAppStatic.Types (MaxAge (NoMaxAge))

app :: (MonadApp m, MonadUnliftIO m) => m Application
app =
  websocketsOr defaultConnectionOptions
    <$> WebSocket.server
    <*> Http.serve static (API.routes <> Page.routes)

static :: Application
static =
  staticApp $
    -- TODO: enable caching
    (defaultWebAppSettings "./client/static/") {ssMaxAge = NoMaxAge}

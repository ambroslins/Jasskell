module Jasskell.Server.API (routes) where

import Jasskell.Server.App (MonadApp, createTable)
import Jasskell.Server.Http qualified as Http
import UnliftIO (MonadUnliftIO)
import Web.Twain (json)

routes :: (MonadUnliftIO m, MonadApp m) => [Http.Route m]
routes =
  [ Http.post "/api/table" (json <$> lift createTable)
  ]

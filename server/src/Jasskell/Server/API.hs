module Jasskell.Server.API (routes) where

import Jasskell.Server.App (createTable)
import Jasskell.Server.Http qualified as Http
import Web.Twain (json)

routes :: [Http.Route]
routes =
  [ Http.post "/api/table" (json <$> createTable)
  ]

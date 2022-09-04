module Jasskell.Server.API where

import Jasskell.Server.TableID (TableID)
import Servant (JSON, NamedRoutes, Post, type (:>))
import Servant.API.Generic (type (:-))

type API = "api" :> NamedRoutes NamedAPI

newtype NamedAPI mode = NamedAPI
  { tableRouts :: mode :- "table" :> NamedRoutes TableRouts
  }
  deriving (Generic)

newtype TableRouts mode = TableRouts
  { postTable :: mode :- Post '[JSON] TableID
  }
  deriving (Generic)

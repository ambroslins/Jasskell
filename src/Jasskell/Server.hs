module Jasskell.Server
  ( Server,
    ServerConfig (..),
    new,
    run,
  )
where

import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Jasskell.Table (Table)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Web.Twain qualified as Twain

data ServerConfig = ServerConfig
  { port :: !Int
  }

data Server = Server
  { config :: !ServerConfig,
    tables :: !(TVar (IntMap Table))
  }

new :: ServerConfig -> IO Server
new c = Server c <$> STM.newTVarIO IntMap.empty

run :: Server -> IO ()
run server@Server {config} =
  Warp.run config.port $
    foldr
      ($)
      (Twain.notFound $ Twain.send $ Twain.html "Not found...")
      (websocketsOr WS.defaultConnectionOptions websocketApp : routes)

websocketApp :: WS.ServerApp
websocketApp pending = pure ()

routes :: [Twain.Middleware]
routes =
  [ Twain.get "/" getRoot
  ]

getRoot :: Twain.ResponderM ()
getRoot = do
  Twain.send $ Twain.html "Hi from Jass!"

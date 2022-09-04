module Main where

import Jasskell.Server (app)
import Jasskell.Server.ServerState qualified as ServerState
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  serverState <- ServerState.empty
  run 8080 (app serverState)

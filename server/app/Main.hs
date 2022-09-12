module Main where

import Jasskell.Server (app)
import Jasskell.Server.Env qualified as Env
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  env <- Env.init
  run 8080 (app env)

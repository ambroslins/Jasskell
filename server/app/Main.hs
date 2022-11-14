module Main where

import Jasskell.Server (app)
import Jasskell.Server.App qualified as App
import Network.Wai.Handler.Warp (run)
import Colog.Actions (richMessageAction)

main :: IO ()
main = do
  env <- App.makeEnv richMessageAction
  App.runAppT env app >>= run 8080

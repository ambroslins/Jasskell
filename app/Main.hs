module Main where

import Env qualified
import Jasskell.Server (ServerConfig (ServerConfig))
import Jasskell.Server qualified as Server

main :: IO ()
main = do
  config <-
    Env.parse (Env.header "jasskell 0.1.0") $
      ServerConfig <$> Env.var Env.auto "PORT" (Env.def 8080)
  server <- Server.new config
  Server.run server

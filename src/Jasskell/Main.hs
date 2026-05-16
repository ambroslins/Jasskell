module Jasskell.Main where

import Env qualified as Envparse
import Jasskell.App (Env (..))
import Jasskell.Logger (Level (..), withStderrLogger)
import Jasskell.Server qualified as Server
import Jasskell.Session qualified as Session
import Jasskell.Table qualified as Table
import Network.Wai.Handler.Warp qualified as Warp

newtype Config = Config
  { port :: Int
  }

configParser :: Envparse.Parser Envparse.Error Config
configParser =
  Config <$> Envparse.var Envparse.auto "PORT" (Envparse.def 8080)

main :: IO ()
main = do
  config <- Envparse.parse (Envparse.header "jasskell 0.1.0") configParser
  withStderrLogger Debug $ \logger -> do
    let env = Env {logger}
    sessionRegistry <- Session.newRegistry
    Table.withManager $ \tableManager ->
      Warp.run config.port $ Server.application env sessionRegistry tableManager

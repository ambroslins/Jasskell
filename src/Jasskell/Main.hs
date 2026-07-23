module Jasskell.Main where

import Control.Monad.Reader (runReaderT)
import Env (auto, def, var)
import Env qualified as Envparse
import Jasskell.App (Env (..))
import Jasskell.Database qualified as DB
import Jasskell.Database.Migration (runMigrations)
import Jasskell.Logger (Level (..), withStderrLogger)
import Jasskell.Server qualified as Server
import Jasskell.Session qualified as Session
import Jasskell.Table qualified as Table
import Network.Wai.Handler.Warp qualified as Warp

data Config = Config
  { port :: !Int,
    database :: DB.Config
  }

configParser :: Envparse.Parser Envparse.Error Config
configParser =
  Config
    <$> var auto "PORT" (def 8080)
    <*> parseDatabaseConfig
  where
    parseDatabaseConfig =
      DB.Config
        <$> var auto "DATABASE_HOST" (def "localhost")
        <*> var auto "DATABASE_PORT" (def 5432)
        <*> var auto "DATABASE_USER" (def "postgres")
        <*> var auto "DATABASE_PASSWORD" (def "postgres")
        <*> var auto "DATABASE_NAME" (def "jaskell")
        <*> var auto "DATABASE_POOL_SIZE" (def 3)

main :: IO ()
main = do
  config <- Envparse.parse (Envparse.header "jasskell 0.1.0") configParser
  withStderrLogger Debug $ \logger ->
    DB.withPool config.database $ \pool -> do
      runReaderT (runMigrations pool) logger
      let env = Env {logger}
      sessionRegistry <- Session.newRegistry
      Table.withManager $ \tableManager ->
        Warp.run config.port $ Server.application env sessionRegistry tableManager

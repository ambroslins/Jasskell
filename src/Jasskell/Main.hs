{-# LANGUAGE ApplicativeDo #-}

module Jasskell.Main (Config, configParser, main) where

import Control.Concurrent.Async qualified as Async
import Control.Exception (SomeAsyncException (..), fromException, handle, throwIO)
import Control.Monad (forever)
import Data.Bits qualified as Bits
import Env (auto, def, var)
import Env qualified as Envparse
import Hasql.Pipeline qualified
import Jasskell.Clock (MonotonicClock, SystemClock)
import Jasskell.Clock qualified as Clock
import Jasskell.Database qualified as DB
import Jasskell.Database.Migration (runMigrations)
import Jasskell.Game qualified as Game
import Jasskell.Limiter qualified as Limiter
import Jasskell.Logger (Level (..), Logger, withStderrLogger, (=:))
import Jasskell.Player qualified as Player
import Jasskell.Server qualified as Server
import Network.Wai.Handler.Warp qualified as Warp

data Config = Config
  { port :: Int,
    database :: DB.Config,
    server :: Server.Config
  }

configParser :: Envparse.Parser Envparse.Error Config
configParser =
  Config
    <$> var auto "PORT" (def 8080)
    <*> parseDatabaseConfig
    <*> parseServerConfig
  where
    parseDatabaseConfig = do
      host <- var auto "DATABASE_HOST" (def "localhost")
      port <- var auto "DATABASE_PORT" (def 5432)
      user <- var auto "DATABASE_USER" (def "postgres")
      password <- var auto "DATABASE_PASSWORD" (def "postgres")
      name <- var auto "DATABASE_NAME" (def "jasskell")
      poolSize <- var auto "DATABASE_POOL_SIZE" (def 3)
      pure $ DB.Config {host, port, user, password, name, poolSize}
    parseServerConfig = do
      limiter <- parseLimiterConfig
      limiterHeader <- var auto "LIMITER_HEADER" (def "X-Forwareded-For")
      pure $ Server.Config {limiter, limiterHeader}
    parseLimiterConfig = do
      slots <- var auto "LIMITER_SLOTS" (def $ 1 `Bits.shiftL` 17)
      rate <- var auto "LIMITER_RATE" (def 1.0)
      burst <- var auto "LIMITER_BURST" (def 50)
      pure $ Limiter.Config {slots, rate, burst}

main :: IO ()
main = do
  config <- Envparse.parse (Envparse.header "jasskell 0.1.0") configParser
  let monotonicClock = Clock.monotonic
      systemClock = Clock.system
  withStderrLogger systemClock Debug $ \logger ->
    DB.withPool config.database $ \db -> do
      runMigrations monotonicClock logger db
      Game.withManager monotonicClock $ \mangager ->
        Async.race_ (cleaner logger monotonicClock systemClock db) $ do
          application <- Server.application logger monotonicClock db config.server mangager
          Warp.run config.port application

cleaner :: Logger -> MonotonicClock -> SystemClock -> DB.Pool -> IO ()
cleaner logger monotonicClock systemClock db = forever $ handle handler $ do
  Clock.sleep monotonicClock interval
  now <- Clock.getSystemTime systemClock
  (duration, ()) <-
    Clock.timed monotonicClock $
      DB.pipeline db $
        Hasql.Pipeline.statement now Player.deleteExpiredSessions
          *> Hasql.Pipeline.statement now Player.deleteExpiredPlayers
  logger.debug "cleaner done" ["duration" =: duration]
  where
    interval = Clock.minutes 5
    handler e
      | Just (SomeAsyncException _) <- fromException e = throwIO e
      | otherwise = logger.error "cleaner crashed" ["exception" =: show e]

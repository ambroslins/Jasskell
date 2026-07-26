{-# LANGUAGE QuasiQuotes #-}

module Jasskell.Database.Migration (runMigrations) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stack (HasCallStack)
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH (resultlessStatement, singletonStatement)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions
  ( IsolationLevel (ReadCommitted),
    Mode (Write),
    transactionNoRetry,
  )
import Jasskell.Database qualified as DB
import Jasskell.Logger
import UnliftIO.Exception (throwIO)

data Migration = Migration
  { name :: !Text,
    sql :: !ByteString
  }

runMigrations :: (HasCallStack, MonadIO m, MonadLogger m) => DB.Pool -> m ()
runMigrations pool = do
  DB.use throwIO pool createSchemaMigrationsTable
  forM_ (zip [1 ..] migrations) $ \(s, m) -> do
    start <- liftIO getMonotonicTimeNSec
    applied <- DB.use throwIO pool $ transactionNoRetry ReadCommitted Write $ do
      Transaction.sql "select pg_advisory_xact_lock(5432)"
      already <- Transaction.statement s isAlreadyApplied
      if already
        then pure False
        else do
          Transaction.sql m.sql
          Transaction.statement (s, m.name) insertMigration
          pure True
    end <- liftIO getMonotonicTimeNSec
    let !durationMs = fromIntegral (end - start) * 1e-6 :: Double
    if applied
      then
        logInfo
          "migration applied"
          ["seq" =: s, "name" =: m.name, "duration_ms" =: durationMs]
      else
        logDebug
          "skipping migration: already applied"
          ["seq" =: s, "name" =: m.name, "duration_ms" =: durationMs]
  logInfo "migrations done" ["seq" =: length migrations]

migrations :: [Migration]
migrations =
  [ Migration
      "create sessions"
      """
      create table sessions (
        id int8 primary key, -- public session id
        secret_sha256 bytea not null,
        nickname text not null,
        created_at timestamptz not null default now(),
        expires_at timestamptz not null default now() + '14 day'::interval
      );
      """
  ]

createSchemaMigrationsTable :: Hasql.Session ()
createSchemaMigrationsTable =
  Hasql.script
    """
    create table if not exists schema_migrations (
      seq int8 not null primary key,
      name text not null,
      applied_at timestamptz not null default now()
    )
    """

insertMigration :: Hasql.Statement (Int64, Text) ()
insertMigration =
  [resultlessStatement|
    insert into schema_migrations (seq, name, applied_at) values ($1::int8, $2::text, now()) |]

isAlreadyApplied :: Hasql.Statement Int64 Bool
isAlreadyApplied =
  [singletonStatement| select exists (select 1 from schema_migrations where seq = $1::int8)::bool |]

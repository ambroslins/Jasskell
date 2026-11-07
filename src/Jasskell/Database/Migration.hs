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

data Migration = Migration
  { name :: !Text,
    sql :: !ByteString
  }

runMigrations :: (HasCallStack) => Logger -> DB.Pool -> IO ()
runMigrations logger pool = do
  DB.use pool createSchemaMigrationsTable
  forM_ (zip [1 ..] migrations) $ \(s, m) -> do
    start <- liftIO getMonotonicTimeNSec
    applied <- DB.use pool $ transactionNoRetry ReadCommitted Write $ do
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
        info = ["seq" =: s, "name" =: m.name, "duration_ms" =: durationMs]
    if applied
      then logger.info "migration applied" info
      else logger.debug "skipping migration: already applied" info
  logger.info "migrations done" ["seq" =: length migrations]

migrations :: [Migration]
migrations =
  [ Migration
      "create players"
      """
      -- A player IS a session. There are no accounts.
      create table players (
        player_id int8 primary key,
        secret_sha256 bytea not null, -- session secret hash
        nickname text not null,
        created_at timestamptz not null default now(),
        expires_at timestamptz not null default now() + '30 days'::interval
      );
      """,
    Migration
      "create games"
      """
      create table games (
        game_id int8 primary key,
        created_by int8 references players(player_id) on delete set null,
        created_at timestamptz not null default now()
      )
      """,
    Migration
      "create seats"
      """
      create table seats (
        game_id int8 not null references games(game_id) on delete cascade,
        seat int4 not null check (seat >= 0),
        player_id int8 references players(player_id) on delete set null,

        primary key (game_id, seat),
        unique (game_id, player_id)
      )
      """,
    Migration
      "create rounds"
      """
      create table rounds (
        game_id int8 not null references games(game_id) on delete cascade,
        round_number int2 not null check (round_number >= 1),
        chooser int2 not null check (chooser >= 0),
        variant int2,
        initial_hands int8[] not null,

        started_at timestamptz not null default now(),

        primary key(game_id, round_number)
      )
      """,
    Migration
      "create played_cards"
      """
      create table played_cards (
        game_id int8 not null,
        round_number int2 not null,
        seq int2 not null check (seq >= 0) ,
        card int2 not null,

        played_at timestamptz not null default now(),

        primary key(game_id, round_number, seq),
        foreign key (game_id, round_number) references rounds on delete cascade
      )
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

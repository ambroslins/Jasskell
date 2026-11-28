{-# LANGUAGE QuasiQuotes #-}

module Jasskell.Database.Migration (runMigrations) where

import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
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
import Jasskell.Clock (MonotonicClock)
import Jasskell.Clock qualified as Clock
import Jasskell.Database qualified as DB
import Jasskell.Logger

data Migration = Migration
  { name :: !Text,
    sql :: !ByteString
  }

runMigrations :: (HasCallStack) => MonotonicClock -> Logger -> DB.Pool -> IO ()
runMigrations clock logger pool = do
  DB.use pool createSchemaMigrationsTable
  forM_ (zip [1 ..] migrations) $ \(s, m) -> do
    (duration, applied) <- Clock.timed clock $ DB.use pool $ transactionNoRetry ReadCommitted Write $ do
      Transaction.sql "select pg_advisory_xact_lock(5432)"
      already <- Transaction.statement s isAlreadyApplied
      unless already $ do
        Transaction.sql m.sql
        Transaction.statement (s, m.name) insertMigration
      pure $ not already
    let info = ["seq" =: s, "name" =: m.name, "duration" =: duration]
    if applied
      then logger.info "migration applied" info
      else logger.debug "skipping migration: already applied" info
  logger.info "migrations done" ["seq" =: length migrations]

migrations :: [Migration]
migrations =
  [ Migration
      "create players"
      """
      create table players (
        player_id int8 primary key,
        nickname text not null,
        created_at timestamptz not null default now()
      );
      """,
    Migration
      "create sessions"
      """
      create table sessions (
        session_id int8 primary key,
        player_id int8 references players(player_id) on delete cascade,
        secret_sha256 bytea not null, -- session secret hash
        created_at timestamptz not null default now(),
        expires_at timestamptz not null default now() + interval '7 days'
      );
      """,
    Migration
      "create games"
      """
      create table games (
        game_id int8 primary key,
        created_by int8 references players(player_id) on delete set null,
        private bool not null default false,
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
        shoved bool not null default false, -- chooser passed the choice to their partner
        skipped_melds int2 not null default 0, -- bitset of seats that didn't announce their Weis
        variant int2,
        initial_hands int8[] not null,
        points int4[], -- total per team (cards, Weis, Stoeck) once finished

        started_at timestamptz not null default now(),
        finished_at timestamptz,

        primary key(game_id, round_number),
        check ((points is null) = (finished_at is null)),
        check (points is null or cardinality(points) = 2)
      )
      """,
    Migration
      "create played_cards"
      """
      create table played_cards (
        game_id int8 not null,
        round_number int2 not null,
        seq int2 not null check (seq >= 0 and seq < 36) ,
        card int2 not null check (card >= 0 and seq < 36),

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

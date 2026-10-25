{-# LANGUAGE QuasiQuotes #-}

module Jasskell.Table
  ( TableManager,
    withManager,
    TableId,
    PublicTable (..),
    create,
    getPublicTables,
    join,
    Connection (..),
    Message (..),
    Command (..),
    WaitingView (..),
    PlayerView (..),
    SpectatorView (..),
  )
where

import Control.Concurrent (forkIOWithUnmask)
import Control.Concurrent.MVar (MVar, modifyMVarMasked, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Exception (SomeException (..), bracket, evaluate, try)
import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap)
import Data.Fin (Fin)
import Data.Fin qualified as Fin
import Data.IntMap.Coercible (IntMap)
import Data.IntMap.Coercible qualified as IntMap
import Data.Maybe (isJust, isNothing)
import Data.Profunctor (Profunctor (lmap), dimap)
import Data.Time (UTCTime, getCurrentTime)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Vec (Vec)
import Data.Vec qualified as Vec
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, sameNat)
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH (maybeStatement, resultlessStatement, rowsAffectedStatement, vectorStatement)
import Jasskell.Card (Card)
import Jasskell.Database qualified as DB
import Jasskell.GameState (GameState, GameView)
import Jasskell.GameState qualified as GameState
import Jasskell.Id (Id (..))
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Player (Nickname (..), Player (..), PlayerId)
import Jasskell.Player qualified as Player
import Jasskell.Throw
import Jasskell.Variant (Variant)
import System.Random qualified as Random

type TableId = Id Table

data Some f = forall n. (KnownNat n) => Some (f n)

newtype TableManager = TableManager
  { tables :: MVar (IntMap TableId (Some Table))
  }

data Table n = Table
  { id :: TableId,
    inbox :: STM.TBQueue (Input n),
    clients :: STM.TVar (IntMap PlayerId (Client n)),
    closed :: STM.TVar Bool
  }
  deriving (Eq)

withManager :: (TableManager -> IO a) -> IO a
withManager f = do
  tm <- TableManager <$> newMVar IntMap.empty
  -- TODO: reaper thread
  -- TODO: stop all tables on cleanup
  f tm

data Client n = Client
  { player :: Player,
    state :: ClientState n
  }
  deriving (Eq)

data ClientState n
  = Connected (STM.TMVar (Message n))
  | Disconnected UTCTime
  deriving (Eq)

data Connection n = Connection
  { receive :: STM (Message n),
    send :: Command n -> STM ()
  }

data Command n
  = TakeSeat (Fin n)
  | StartGame
  | DeclareVariant Variant
  | PlayCard Card
  deriving (Eq, Show, Generic)

data Message n
  = UpdateWaiting (WaitingView n)
  | UpdatePlayer (PlayerView n)
  | UpdateSpectator SpectatorView
  | Rejected (Rejection n)
  | Superseded
  | TableClosed
  deriving (Show)

data Rejection n
  = SeatTaken
  | AlreadySeated
  | NotAPlayer
  | VariantAlreadyDeclared
  | GameAlreadyStarted
  | GameNotStarted
  | NotYourTurn
  | IllegalCard (GameState.IllegalCard n)
  deriving (Show)

data Input n
  = PlayerJoined PlayerId
  | PlayerLeft PlayerId
  | PlayerCommand PlayerId (Command n)
  deriving (Eq, Show)

create :: DB.Pool -> PlayerId -> IO TableId
create db createdBy = do
  tableId <- Id.new
  DB.use db $ Hasql.statement (tableId, createdBy) insertTable
  pure tableId

insertTable :: Hasql.Statement (TableId, PlayerId) ()
insertTable =
  lmap
    (bimap Id.toInt64 Id.toInt64)
    [resultlessStatement|
      insert into tables (table_id, created_by)
      values ($1::int8, $2::int8)
    |]

data PublicTable = PublicTable
  { id :: TableId,
    creatorName :: Nickname,
    seatsTaken :: Int
  }
  deriving (Show)

getPublicTables :: DB.Pool -> IO (Vector PublicTable)
getPublicTables db = DB.use db $ Hasql.statement () selectPublicTables

-- TODO: ignore tables with active games (or put them at the end)
selectPublicTables :: Hasql.Statement () (Vector PublicTable)
selectPublicTables =
  fmap
    ( Vector.map $ \(tableId, nickname, seatsTaken) ->
        PublicTable
          { id = Id.fromInt64 tableId,
            creatorName = Nickname nickname,
            seatsTaken = fromIntegral seatsTaken
          }
    )
    [vectorStatement|
      select
        t.table_id::int8,
        any_value(p.nickname)::text,
        count(s.table_id)::int8 as seats_taken
      from tables as t
      join players as p on p.player_id = t.created_by
      left join seats as s on s.table_id = t.table_id
      group by t.table_id
      order by seats_taken desc, t.table_id asc
    |]

join ::
  Logger ->
  DB.Pool ->
  Player ->
  TableId ->
  TableManager ->
  (forall n. (KnownNat n) => Connection n -> IO a) ->
  IO (Maybe a)
join logger db player tableId manager handler = handling pure $ \notFound -> do
  tables <- readMVar manager.tables
  Some table <- case IntMap.lookup tableId tables of
    Nothing ->
      getTableSnapshot db tableId >>= \case
        Nothing -> throw notFound Nothing
        Just (Some snapshot) -> Some <$> spawnTable logger db snapshot manager
    Just t -> pure t
  messageBox <- STM.newEmptyTMVarIO
  let client = Client {player, state = Connected messageBox}
      connection =
        Connection
          { receive = do
              closed <- STM.readTVar table.closed
              if closed
                then pure TableClosed
                else STM.takeTMVar messageBox,
            send = STM.writeTBQueue table.inbox . PlayerCommand player.id
          }
      acquire = STM.atomically $ do
        clients <- STM.readTVar table.clients
        case IntMap.lookup player.id clients of
          Just Client {state = Connected box} -> STM.writeTMVar box Superseded
          _ -> pure ()
        STM.writeTVar table.clients $!
          IntMap.insert player.id client clients
        STM.writeTBQueue table.inbox $ PlayerJoined player.id
      release _ = do
        disconnected <- Disconnected <$> getCurrentTime
        STM.atomically $ do
          STM.modifyTVar' table.clients $
            IntMap.adjust
              (\c -> if c == client then client {state = disconnected} else c)
              player.id
          STM.writeTBQueue table.inbox $ PlayerLeft player.id
  bracket acquire release $ \() -> Just <$> handler connection

spawnTable :: forall n. (KnownNat n) => Logger -> DB.Pool -> Snapshot n -> TableManager -> IO (Table n)
spawnTable l db snapshot manager = do
  inbox <- STM.newTBQueueIO 16
  clients <- STM.newTVarIO @(IntMap PlayerId (Client n)) IntMap.empty
  let !state = restoreState snapshot
  modifyMVarMasked manager.tables $ \tables ->
    case IntMap.lookup snapshot.id tables of
      Just (Some table) -> case sameNat snapshot table of
        Nothing -> error "bad nat"
        Just Refl -> pure (tables, table)
      Nothing -> do
        closed <- STM.newTVarIO False
        let !table = Table {id = snapshot.id, inbox, clients, closed} :: Table n
        _threadId <- forkIOWithUnmask $ \unmask -> do
          result <- try $ unmask $ do
            logger.debug "table started" []
            tableLoop logger db table state
          modifyMVar_ manager.tables (evaluate . IntMap.delete snapshot.id)
          STM.atomically $ STM.writeTVar closed True
          unmask $ case result of
            Left (SomeException e) -> logger.error "table crashed" ["exception" =: show e]
            Right () -> logger.debug "table closed" []

        let !ts = IntMap.insert snapshot.id (Some table) tables
        pure (ts, table)
  where
    logger = withContext l ["table_id" =: Id.encodeText snapshot.id]

restoreState :: Snapshot n -> TableState n
restoreState snapshot = Waiting snapshot.seats

data Snapshot n = Snapshot
  { id :: TableId,
    creator :: PlayerId,
    seats :: Vec n (Maybe Player)
  }

getTableSnapshot :: DB.Pool -> TableId -> IO (Maybe (Some Snapshot))
getTableSnapshot db tableId =
  DB.use db (Hasql.statement tableId selectTableSnapshot)

selectTableSnapshot :: Hasql.Statement TableId (Maybe (Some Snapshot))
selectTableSnapshot =
  dimap
    Id.toInt64
    (fmap decode)
    [maybeStatement|
      select
        t.table_id::int8,
        t.created_by::int8,
        coalesce(s.seats, '{}'::int4[])::int4[],
        coalesce(s.player_ids, '{}'::int8[])::int8[],
        coalesce(s.nicknames, '{}'::text[])::text[]
      from tables as t
      left join lateral (
        select
          array_agg(s.seat order by s.seat) as seats,
          array_agg(s.player_id order by s.seat) as player_ids,
          array_agg(p.nickname order by s.seat) as nicknames
        from seats as s
        left join players as p on p.player_id = s.player_id
        where s.table_id = t.table_id and p.player_id is not null
      ) as s on true
      where table_id = $1::int8 and created_by is not null
    |]
  where
    decode (tableId, creatdBy, indices, playerIds, nicknames) =
      Some $
        Snapshot
          { id = Id.fromInt64 tableId,
            creator = Id.fromInt64 creatdBy,
            seats =
              Vector.foldl' setSeat (Vec.replicate Nothing) $
                Vector.zip3 indices playerIds nicknames
          }
    setSeat v (i, p, n) =
      Vec.set @4
        (fromIntegral i)
        (Just $ Player {id = Id.fromInt64 p, nickname = Player.Nickname n})
        v

tableLoop :: (KnownNat n) => Logger -> DB.Pool -> Table n -> TableState n -> IO ()
tableLoop logger db table = go
  where
    go state = do
      input <- STM.atomically $ STM.readTBQueue table.inbox
      logger.debug "got table input" ["input" =: show input]
      clients <- STM.readTVarIO table.clients
      case input of
        PlayerJoined playerId -> do
          case IntMap.lookup playerId clients of
            Just Client {state = Connected box} ->
              STM.atomically $ STM.writeTMVar box $ renderState state playerId
            _ -> pure ()
          go state
        PlayerLeft _ -> go state
        PlayerCommand playerId cmd ->
          go =<< case IntMap.lookup playerId clients of
            Nothing -> pure state
            Just Client {player, state = clientState} ->
              applyCommand db table.id state player cmd >>= \case
                Left rejection -> case clientState of
                  Disconnected _ -> pure state
                  Connected box -> do
                    STM.atomically $ STM.writeTMVar box $ Rejected rejection
                    pure state
                Right s -> do
                  let render = renderState s
                  forM_ clients $ \client -> case client.state of
                    Disconnected _ -> pure ()
                    Connected box -> STM.atomically $ STM.writeTMVar box $ render client.player.id
                  pure s
    renderState state = case state of
      Waiting seats -> \playerId ->
        let isPlayer = maybe False (\p -> p.id == playerId)
         in UpdateWaiting $ waitingViewFor (Vec.findIndex isPlayer seats) seats
      Playing seats gameState -> \playerId ->
        case Vec.findIndex (\p -> p.id == playerId) seats of
          Nothing -> UpdateSpectator $ spectatorView seats gameState
          Just i -> UpdatePlayer $ playerViewFor i seats gameState

applyCommand :: (KnownNat n) => DB.Pool -> TableId -> TableState n -> Player -> Command n -> IO (Either (Rejection n) (TableState n))
applyCommand db tableId state player cmd = trying $ \reject -> case cmd of
  TakeSeat seat -> case state of
    Playing _ _ -> throw reject GameAlreadyStarted
    Waiting seats
      | isJust playerSeat -> throw reject AlreadySeated
      | isJust (Vec.index seats seat) -> throw reject SeatTaken
      | otherwise -> do
          ok <- DB.use db $ Hasql.statement (tableId, player.id, seat) insertSeat
          unless ok $ error "seat insert failed"
          pure $ Waiting $ Vec.set seat (Just player) seats
  StartGame -> case state of
    Playing _ _ -> throw reject GameAlreadyStarted
    Waiting seats -> case Vec.sequence seats of
      Nothing -> error "table not full"
      Just s
        | isNothing playerSeat -> throw reject NotAPlayer
        | otherwise -> do
            stdGen <- Random.newStdGen
            let gameState = GameState.new stdGen
            -- TODO: Save game to database
            pure $ Playing s gameState
  DeclareVariant variant -> case state of
    Waiting _ -> throw reject GameNotStarted
    Playing seats gameState -> do
      checkCurrentPlayer reject gameState
      case GameState.declareVariant variant gameState of
        Left _ -> throw reject VariantAlreadyDeclared
        Right gs -> pure $ Playing seats gs
  PlayCard card -> case state of
    Waiting _ -> throw reject GameNotStarted
    Playing seats gameState -> do
      checkCurrentPlayer reject gameState
      case GameState.playCard card gameState of
        Left illegal -> throw reject $ IllegalCard illegal
        Right gs -> do
          -- TODO: save played card event
          pure $ Playing seats gs
  where
    playerSeat = case state of
      Waiting seats -> Vec.findIndex (maybe False $ \p -> p.id == player.id) seats
      Playing seats _ -> Vec.findIndex (\p -> p.id == player.id) seats
    checkCurrentPlayer reject gs = case playerSeat of
      Nothing -> throw reject NotYourTurn
      Just s
        | s /= GameState.currentPlayer gs -> throw reject NotYourTurn
        | otherwise -> pure ()

insertSeat :: Hasql.Statement (TableId, PlayerId, Fin n) Bool
insertSeat =
  dimap
    encode
    (> 0)
    [rowsAffectedStatement|
      insert into seats (table_id, player_id, seat)
      values ($1::int8, $2::int8, $3::int4)
      on conflict (table_id, seat) do update
        set player_id = excluded.player_id
      where seats.player_id is null
    |]
  where
    encode (tableId, playerId, seat) =
      (Id.toInt64 tableId, Id.toInt64 playerId, fromIntegral $ Fin.toInt seat)

data TableState n
  = Waiting (Vec n (Maybe Player))
  | Playing (Vec n Player) (GameState n)
  deriving (Show)

data WaitingView n = WaitingView
  { seats :: Vec n (Maybe Nickname),
    yourSeat :: Maybe (Fin n)
  }
  deriving (Show)

waitingViewFor :: (KnownNat n) => Maybe (Fin n) -> Vec n (Maybe Player) -> WaitingView n
waitingViewFor perspective seats =
  WaitingView
    { seats = Vec.map (fmap (.nickname)) seats,
      yourSeat = perspective
    }

data PlayerView n = PlayerView
  { seats :: Vec n Nickname,
    game :: GameView n
  }
  deriving (Show)

playerViewFor :: (KnownNat n) => Fin n -> Vec n Player -> GameState n -> PlayerView n
playerViewFor p seats gameState =
  PlayerView
    { seats = Vec.rotate p $ Vec.map (.nickname) seats,
      game = GameState.viewFor p gameState
    }

data SpectatorView = SpectatorView
  deriving (Show)

spectatorView :: Vec n Player -> GameState n -> SpectatorView
spectatorView _ _ = SpectatorView

instance (KnownNat n) => ToJSON (Command n) where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance (KnownNat n) => FromJSON (Command n) where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

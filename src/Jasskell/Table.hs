{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Concurrent.STM qualified as STM
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.TH qualified
import Data.Bifunctor (bimap)
import Data.IntMap.Coercible (IntMap)
import Data.IntMap.Coercible qualified as IntMap
import Data.Maybe (isNothing)
import Data.Profunctor (Profunctor (lmap), dimap)
import Data.Time (UTCTime, getCurrentTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector4 (Vector4)
import Data.Vector4 qualified as Vector4
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH (maybeStatement, resultlessStatement, vectorStatement)
import Jasskell.App
import Jasskell.Card (Card)
import Jasskell.GameState (GameState, GameView)
import Jasskell.GameState qualified as GameState
import Jasskell.Id (Id (..))
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Player (Nickname (..), Player (..), PlayerId)
import Jasskell.Player qualified as Player
import Jasskell.Variant (Variant)
import System.Random qualified as Random
import UnliftIO (MonadUnliftIO, bracket, finally)
import UnliftIO.Async (Async, asyncWithUnmask)
import UnliftIO.Exception (evaluate)
import UnliftIO.MVar
  ( MVar,
    modifyMVarMasked,
    modifyMVar_,
    newMVar,
    readMVar,
  )
import UnliftIO.STM
  ( STM,
    TBQueue,
    TMVar,
    TVar,
    atomically,
    newEmptyTMVarIO,
    newTBQueueIO,
    newTVarIO,
    readTVarIO,
  )

type TableId = Id Handle

newtype TableManager = TableManager
  { tables :: MVar (IntMap TableId Handle)
  }

data Handle = Handle
  { tableId :: TableId,
    inputQueue :: TBQueue Input,
    clients :: TVar (IntMap PlayerId ClientState),
    thread :: Async ()
  }
  deriving (Eq)

data Table = Table
  { id :: TableId,
    creator :: PlayerId,
    seats :: Vector4 (Maybe Player)
  }

withManager :: (MonadIO m) => (TableManager -> m a) -> m a
withManager f = do
  tm <- TableManager <$> newMVar IntMap.empty
  -- TODO: reaper thread
  -- TODO: stop all tables on cleanup
  f tm

data Client = Client
  { player :: Player,
    messageBox :: TMVar Message
  }
  deriving (Eq)

data ClientState
  = Connected Client
  | Disconnected UTCTime

data Connection = Connection
  { receive :: STM Message,
    send :: Command -> STM ()
  }

data Command
  = TakeSeat Vector4.Index4
  | StartGame
  | DeclareVariant Variant
  | PlayCard Card
  deriving (Eq, Show)

data Message
  = UpdateWaiting WaitingView
  | UpdatePlayer PlayerView
  | UpdateSpectator SpectatorView
  | ConnectionClosed
  deriving (Show)

data Input
  = PlayerJoined Player
  | PlayerLeft Player
  | PlayerCommand Player Command
  deriving (Eq, Show)

create :: (MonadIO m) => PlayerId -> AppT m TableId
create createdBy = do
  tableId <- Id.new
  useDB $ Hasql.statement (tableId, createdBy) insertTable
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

getPublicTables :: (MonadIO m) => AppT m (Vector PublicTable)
getPublicTables = useDB $ Hasql.statement () selectPublicTables

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
  (MonadUnliftIO m) =>
  Player ->
  TableId ->
  TableManager ->
  (Maybe Connection -> AppT m a) ->
  AppT m a
join player tableId manager handler = do
  tables <- readMVar manager.tables
  case IntMap.lookup tableId tables of
    Nothing ->
      spawnTable tableId manager >>= \case
        Nothing -> handler Nothing
        Just t -> go t
    Just t -> go t
  where
    go table = do
      messageBox <- newEmptyTMVarIO
      let client = Client {player, messageBox}
          connection =
            Connection
              { receive = STM.takeTMVar messageBox,
                send = STM.writeTBQueue table.inputQueue . PlayerCommand player
              }
          acquire = atomically $ do
            clients <- STM.readTVar table.clients
            case IntMap.lookup player.id clients of
              Nothing -> pure ()
              Just _old -> undefined -- TODO: force leave player
            STM.writeTVar table.clients $!
              IntMap.insert player.id (Connected client) clients
            STM.writeTBQueue table.inputQueue $ PlayerJoined player
          release _ = do
            lastSeen <- liftIO getCurrentTime
            let disconnect = \case
                  Connected c | c == client -> Disconnected lastSeen
                  cs -> cs
            atomically $ do
              STM.modifyTVar' table.clients $
                IntMap.adjust disconnect player.id
              STM.writeTBQueue table.inputQueue $ PlayerLeft player
      bracket acquire release $ \() -> handler (Just connection)

spawnTable :: (MonadUnliftIO m) => TableId -> TableManager -> AppT m (Maybe Handle)
spawnTable tableId manager =
  useDB (Hasql.statement tableId selectTableById) >>= \case
    Nothing -> pure Nothing
    Just table -> do
      inputQueue <- newTBQueueIO 16
      clients <- newTVarIO IntMap.empty
      modifyMVarMasked manager.tables $ \tables ->
        case IntMap.lookup tableId tables of
          Just handle -> pure (tables, Just handle)
          Nothing -> do
            let deregister = do
                  modifyMVar_ manager.tables (evaluate . IntMap.delete tableId)
                  cs <- readTVarIO clients
                  forM_ cs $ \case
                    Connected c -> atomically $ STM.writeTMVar c.messageBox ConnectionClosed
                    Disconnected _ -> pure ()
            thread <- asyncWithUnmask $ \unmask ->
              unmask (tableLoop table inputQueue clients) `finally` deregister
            let !handle = Handle {tableId, inputQueue, clients, thread}
                !ts = IntMap.insert tableId handle tables
            pure (ts, Just handle)

selectTableById :: Hasql.Statement TableId (Maybe Table)
selectTableById =
  dimap
    Id.toInt64
    ( fmap $ \(tableId, createdBy, is, playerIds, nicknames) ->
        Table
          { id = Id.fromInt64 tableId,
            creator = Id.fromInt64 createdBy,
            seats =
              Vector.foldl'
                ( \v (i, playerId, nickname) ->
                    Vector4.set
                      (fromIntegral i)
                      ( Just $
                          Player
                            { id = Id.fromInt64 playerId,
                              nickname = maybe "" Player.Nickname nickname
                            }
                      )
                      v
                )
                (Vector4.replicate Nothing)
                $ Vector.zip3 is playerIds nicknames
          }
    )
    [maybeStatement|
      select
        t.table_id::int8,
        t.created_by::int8,
        coalesce(s.indices, '{}'::int4[])::int4[],
        coalesce(s.player_ids, '{}'::int8[])::int8[],
        coalesce(s.nicknames, '{}'::text[])::text?[]
      from tables as t
      left join lateral (
        select
          array_agg(s.index order by s.index) as indices,
          array_agg(s.player_id order by s.index) as player_ids,
          array_agg(p.nickname order by s.index) as nicknames
        from seats as s
        left join players as p on p.player_id = s.player_id
        where s.table_id = t.table_id
      ) as s on true
      where table_id = $1::int8
    |]

tableLoop :: (MonadIO m) => Table -> TBQueue Input -> TVar (IntMap PlayerId ClientState) -> AppT m ()
tableLoop table inputQueue clientsVar = go initial
  where
    initial = Waiting table.seats
    broadcast clients makeMsg = do
      IntMap.forWithKey_ clients $ \playerId cs -> case cs of
        Connected client -> atomically $ STM.writeTMVar client.messageBox (makeMsg playerId)
        Disconnected _ -> pure ()
    go state = do
      input <- atomically $ STM.readTBQueue inputQueue
      logDebug "got table input" ["input" =: show input]
      s <- case state of
        Waiting seats -> case input of
          PlayerJoined player
            | player.id == table.creator && all isNothing seats -> do
                useDB $ Hasql.statement (table.id, player.id, 0) insertSeat
                pure $ Waiting $ Vector4.set 0 (Just player) seats
            | otherwise -> pure state
          PlayerLeft _ -> pure state
          PlayerCommand player cmd -> case cmd of
            TakeSeat seat
              | any (maybe False $ \p -> p.id == player.id) seats -> error "already seated" -- TODO: already seated
              | Just _ <- Vector4.index seat seats -> error "already taken" -- TODO: already taken
              | otherwise -> do
                  useDB $ Hasql.statement (table.id, player.id, seat) insertSeat
                  pure $ Waiting $ Vector4.set seat (Just player) seats
            StartGame -> case sequence seats of
              Nothing -> error "game not full"
              Just takenSeats -> do
                stdGen <- Random.newStdGen
                let gameState = GameState.new stdGen
                pure $ Playing takenSeats gameState
            _ -> pure state -- TODO: invalid command
        Playing seats gameState -> case input of
          PlayerCommand player cmd
            | (Vector4.index (GameState.currentPlayer gameState) seats).id /= player.id -> error "not the current player"
            | otherwise -> case cmd of
                DeclareVariant variant -> case GameState.declareVariant variant gameState of
                  Left e -> error $ "declare variant: " <> show e
                  Right gs -> pure $ Playing seats gs
                PlayCard card -> case GameState.playCard card gameState of
                  Left e -> error $ "play card: " <> show e
                  Right gs -> do
                    logDebug "card played" ["gameState" =: show gs]
                    pure $ Playing seats gs
          _ -> pure state

      clients <- readTVarIO clientsVar
      broadcast clients $ case s of
        Waiting seats -> \playerId ->
          let isPlayer = maybe False (\p -> p.id == playerId)
           in UpdateWaiting $ waitingViewFor (Vector4.findIndex isPlayer seats) seats
        Playing seats gameState -> \playerId ->
          case Vector4.findIndex (\p -> p.id == playerId) seats of
            Nothing -> UpdateSpectator $ spectatorView seats gameState
            Just i -> UpdatePlayer $ playerViewFor i seats gameState
      go s

insertSeat :: Hasql.Statement (TableId, PlayerId, Vector4.Index4) ()
insertSeat =
  lmap
    ( \(tableId, playerId, index) ->
        (Id.toInt64 tableId, Id.toInt64 playerId, fromIntegral $ fromEnum index)
    )
    [resultlessStatement|
      insert into seats (table_id, player_id, index)
      values ($1::int8, $2::int8, $3::int8)
    |]

data TableState
  = Waiting (Vector4 (Maybe Player))
  | Playing (Vector4 Player) GameState
  deriving (Show)

data WaitingView = WaitingView
  { seats :: Vector4 (Maybe Nickname),
    yourSeat :: Maybe Vector4.Index4
  }
  deriving (Show)

waitingViewFor :: Maybe Vector4.Index4 -> Vector4 (Maybe Player) -> WaitingView
waitingViewFor perspective seats =
  WaitingView
    { seats = fmap (.nickname) <$> seats,
      yourSeat = perspective
    }

data PlayerView = PlayerView
  { seats :: Vector4 Nickname,
    game :: GameView
  }
  deriving (Show)

playerViewFor :: Vector4.Index4 -> Vector4 Player -> GameState -> PlayerView
playerViewFor p seats gameState =
  PlayerView
    { seats = Vector4.rotate p $ (.nickname) <$> seats,
      game = GameState.viewFor p gameState
    }

data SpectatorView = SpectatorView
  deriving (Show)

spectatorView :: Vector4 Player -> GameState -> SpectatorView
spectatorView _ _ = SpectatorView

$(Data.Aeson.TH.deriveJSON Data.Aeson.TH.defaultOptions ''Command)

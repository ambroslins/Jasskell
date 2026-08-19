{-# LANGUAGE QuasiQuotes #-}

module Jasskell.Table
  ( TableManager,
    withManager,
    TableId,
    Table (id),
    create,
    Connection (..),
    join,
    JoinError (..),
    Message (..),
    Event (..),
    UpdateResult (..),
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Data.IntMap.Coercible (IntMap)
import Data.IntMap.Coercible qualified as IntMap
import Data.Maybe (isNothing)
import Data.Profunctor (Profunctor (lmap), dimap)
import Data.Time (UTCTime, getCurrentTime)
import Data.Vector4 (Vector4)
import Data.Vector4 qualified as Vector4
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH (maybeStatement, resultlessStatement)
import Jasskell.App
import Jasskell.Card (Card)
import Jasskell.GameState (DeclareError, GameState, ShoveError, UnplayableCardReason)
import Jasskell.Id (Id (..))
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Player (Player (..), PlayerId)
import Jasskell.Variant (Variant)
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

type TableId = Id Table

newtype TableManager = TableManager
  { tables :: MVar (IntMap TableId Table)
  }

data Table = Table
  { id :: TableId,
    creator :: PlayerId,
    eventQueue :: TBQueue Event,
    clients :: TVar (IntMap PlayerId ClientState),
    thread :: Async ()
  }
  deriving (Eq)

withManager :: (MonadIO m) => (TableManager -> m a) -> m a
withManager f = do
  tm <- TableManager <$> newMVar IntMap.empty
  -- TODO: reaper thread
  -- TODO: stop all tables on cleanup
  f tm

data Client = Client
  { player :: Player,
    messageBox :: TMVar ServerMessage
  }
  deriving (Eq)

data ClientState
  = Connected Client
  | Disconnected UTCTime

newtype ServerMessage = Snapshot TableState
  deriving (Show)

data ClientMessage
  deriving (Eq, Show)

data Connection = Connection
  { send :: ClientMessage -> STM (),
    receive :: STM ServerMessage
  }

data Event
  = PlayerJoined PlayerId
  | PlayerLeft PlayerId
  | PlayerMessage PlayerId ClientMessage
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

join ::
  (MonadUnliftIO m) =>
  Player ->
  TableId ->
  TableManager ->
  (Either JoinError Connection -> AppT m a) ->
  AppT m a
join player tableId manager handler = do
  tables <- readMVar manager.tables
  case IntMap.lookup tableId tables of
    Nothing ->
      spawnTable tableId manager >>= \case
        Nothing -> handler $ Left TableNotFound
        Just t -> go t
    Just t -> go t
  where
    go table = do
      messageBox <- newEmptyTMVarIO
      let client = Client {player, messageBox}
          send msg = STM.writeTBQueue table.eventQueue (PlayerMessage player.id msg)
          receive = STM.takeTMVar messageBox
          connection = Connection {send, receive}
          acquire = atomically $ do
            clients <- STM.readTVar table.clients
            case IntMap.lookup player.id clients of
              Nothing -> pure ()
              Just _old -> undefined -- TODO: force leave player
            STM.writeTVar table.clients $!
              IntMap.insert player.id (Connected client) clients
            STM.writeTBQueue table.eventQueue $ PlayerJoined player.id
          release _ = do
            lastSeen <- liftIO getCurrentTime
            let disconnect = \case
                  Connected c | c == client -> Disconnected lastSeen
                  cs -> cs
            atomically $ do
              STM.modifyTVar' table.clients $
                IntMap.adjust disconnect player.id
              STM.writeTBQueue table.eventQueue $ PlayerLeft player.id
      bracket acquire release $ \() -> handler (Right connection)

spawnTable :: (MonadUnliftIO m) => TableId -> TableManager -> AppT m (Maybe Table)
spawnTable tableId manager =
  useDB (Hasql.statement tableId selectTableById) >>= \case
    Nothing -> pure Nothing
    Just creator -> do
      eventQueue <- newTBQueueIO 16
      clients <- newTVarIO IntMap.empty
      modifyMVarMasked manager.tables $ \tables ->
        case IntMap.lookup tableId tables of
          Just t -> pure (tables, Just t)
          Nothing -> do
            thread <- asyncWithUnmask $ \unmask ->
              unmask (tableLoop creator eventQueue clients) `finally` deregister
            let !t = Table {id = tableId, creator, eventQueue, clients, thread}
                !ts = IntMap.insert tableId t tables
            pure (ts, Just t)
  where
    deregister =
      modifyMVar_ manager.tables (evaluate . IntMap.delete tableId)

selectTableById :: Hasql.Statement TableId (Maybe PlayerId)
selectTableById =
  dimap
    Id.toInt64
    (fmap Id.fromInt64)
    [maybeStatement|
      select created_by::int8
      from tables where table_id = $1::int8
    |]

tableLoop :: (MonadIO m) => PlayerId -> TBQueue Event -> TVar (IntMap PlayerId ClientState) -> AppT m ()
tableLoop creator eventQueue clientsVar = go initial
  where
    initial = Waiting $ Vector4.replicate Nothing
    broadcast msg = do
      clients <- readTVarIO clientsVar
      IntMap.forWithKey_ clients $ \_userId cs -> case cs of
        Connected client -> atomically $ STM.writeTMVar client.messageBox msg
        Disconnected _ -> pure ()
    go state = do
      event <- atomically $ STM.readTBQueue eventQueue
      logDebug "got event" ["event" =: show event]
      s <- case state of
        Waiting seats -> case event of
          PlayerJoined playerId
            | playerId == creator && all isNothing seats ->
                pure $ Waiting $ Vector4.set 0 (Just playerId) seats
          _ -> pure state
        _ -> pure state

      broadcast $ Snapshot s
      go s

data TableState
  = Waiting (Vector4 (Maybe PlayerId))
  | Playing (Vector4 PlayerId) GameState
  deriving (Show)

data Message
  = DeclareVariant Variant
  | Shove
  | PlayCard Card
  | Disconnect
  deriving (Show)

data UpdateResult
  = Ok
  | NotYourTurn
  | DeclareError !DeclareError
  | ShoveError !ShoveError
  | UnplayableCard !UnplayableCardReason
  deriving (Show)

data JoinError = TableNotFound
  deriving (Show)

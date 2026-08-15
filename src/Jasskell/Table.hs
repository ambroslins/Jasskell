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
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (bimap)
import Data.IntMap.Coercible (IntMap)
import Data.IntMap.Coercible qualified as IntMap
import Data.Profunctor (Profunctor (lmap))
import Data.Vector4 (Vector4)
import Data.Vector4 qualified as Vector4
import Hasql.Session qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH (resultlessStatement)
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
    eventQueue :: TBQueue Event,
    clients :: TVar (IntMap PlayerId Client),
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

newtype ServerMessage = ServerMessage TableState
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
              IntMap.insert player.id client clients
          release _ = atomically $ do
            clients <- STM.readTVar table.clients
            let softDelete c
                  | c.messageBox == client.messageBox = Nothing
                  | otherwise = Just c
            STM.writeTVar table.clients $!
              IntMap.update softDelete player.id clients
      bracket acquire release $ \() -> handler (Right connection)

spawnTable :: (MonadUnliftIO m) => TableId -> TableManager -> AppT m (Maybe Table)
spawnTable tableId manager = do
  -- TODO: query database
  eventQueue <- newTBQueueIO 16
  clients <- newTVarIO IntMap.empty
  modifyMVarMasked manager.tables $ \tables ->
    case IntMap.lookup tableId tables of
      Just t -> pure (tables, Just t)
      Nothing -> do
        thread <- asyncWithUnmask $ \unmask ->
          unmask (tableLoop eventQueue clients) `finally` deregister
        let !t = Table {id = tableId, eventQueue, clients, thread}
            !ts = IntMap.insert tableId t tables
        pure (ts, Just t)
  where
    deregister =
      modifyMVar_ manager.tables (evaluate . IntMap.delete tableId)

tableLoop :: (MonadIO m) => TBQueue Event -> TVar (IntMap PlayerId Client) -> AppT m ()
tableLoop eventQueue clientsVar = go initial
  where
    initial = Waiting $ Vector4.replicate Nothing
    _broadcast msg = do
      clients <- readTVarIO @IO clientsVar
      IntMap.forWithKey_ clients $ \_userId client ->
        atomically $ STM.writeTMVar client.messageBox msg
    go state = do
      event <- atomically $ STM.readTBQueue eventQueue
      logDebug "got event" ["event" =: show event]
      go state

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

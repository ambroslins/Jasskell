module Jasskell.Table
  ( TableManager,
    withManager,
    TableId,
    Table (id),
    new,
    lookup,
    withClient,
    JoinError (..),
    Message (..),
    Event (..),
    UpdateResult (..),
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Coercible (IntMap)
import Data.IntMap.Coercible qualified as IntMap
import Data.Vector4 (Vector4)
import Data.Vector4 qualified as Vector4
import Jasskell.Card (Card)
import Jasskell.GameState (DeclareError, ShoveError, UnplayableCardReason)
import Jasskell.Id (Id (..))
import Jasskell.Id qualified as Id
import Jasskell.Session (Session (..), SessionId)
import Jasskell.Variant (Variant)
import UnliftIO (MonadUnliftIO, bracket)
import Prelude hiding (lookup)

type TableId = Id Table

newtype TableManager = TableManager {tables :: IORef (IntMap TableId Table)}

withManager :: (MonadIO m) => (TableManager -> m a) -> m a
withManager f = do
  tm <- liftIO $ TableManager <$> newIORef IntMap.empty
  -- TODO: reaper thread
  -- TODO: stop all tables on cleanup
  f tm

lookup :: (MonadIO m) => TableId -> TableManager -> m (Maybe Table)
lookup tableId tm = liftIO $ IntMap.lookup tableId <$> readIORef tm.tables

type ClientId = Id Client

data Client = Client
  { id :: ClientId,
    session :: Session,
    messageBox :: STM.TMVar ServerMessage
  }

newtype ServerMessage = ServerMessage TableState
  deriving (Show)

data ClientMessage

data Event
  = PlayerJoined SessionId
  | PlayerLeft SessionId
  | ClientMessage
  deriving (Eq, Show)

data Table = Table
  { id :: TableId,
    eventQueue :: STM.TBQueue Event,
    clients :: STM.TVar (IntMap SessionId Client),
    status :: STM.TVar TableStatus,
    async :: Async ()
  }

new :: (MonadIO m) => TableManager -> m TableId
new tm = liftIO $ do
  tableId <- Id.new
  eventQueue <- STM.newTBQueueIO 16
  clients <- STM.newTVarIO IntMap.empty
  status <- STM.newTVarIO $ Waiting 0
  async <- Async.async $ tableLoop eventQueue clients status
  let table = Table {id = tableId, eventQueue, clients, status, async}
  atomicModifyIORef' tm.tables ((,()) . IntMap.insert tableId table)
  pure tableId

tableLoop :: STM.TBQueue Event -> STM.TVar (IntMap SessionId Client) -> STM.TVar TableStatus -> IO ()
tableLoop eventQueue clientsVar _statusVar = go inital
  where
    inital = TableState {players = Vector4.replicate Nothing}
    broadcast msg = do
      clients <- STM.atomically $ STM.readTVar clientsVar
      IntMap.forWithKey_ clients $ \_userId client ->
        STM.atomically $ STM.writeTMVar client.messageBox msg
    go state = do
      event <- STM.atomically $ STM.readTBQueue eventQueue
      case event of
        PlayerJoined sessionId ->
          let findSeat = \case
                Nothing -> True
                Just s -> s == sessionId
           in case Vector4.findIndex findSeat state.players of
                Nothing -> go state -- spectator
                Just i ->
                  let newState = state {players = Vector4.set i (Just sessionId) state.players}
                   in broadcast (ServerMessage newState) >> go newState

withClient :: (MonadUnliftIO m) => Table -> Session -> ((ClientMessage -> STM ()) -> STM ServerMessage -> m a) -> m a
withClient table session handle = bracket enter leave run
  where
    enter = liftIO $ do
      clientId <- Id.new
      messageBox <- STM.newEmptyTMVarIO
      let client = Client {id = clientId, session, messageBox}
      STM.atomically $ do
        -- TODO: notify old client if necessary
        STM.modifyTVar' table.clients (IntMap.insert session.id client)
        STM.writeTBQueue table.eventQueue (PlayerJoined session.id)
      pure client
    leave client = liftIO $ STM.atomically $ do
      clients <- STM.readTVar table.clients
      let deleteIfSameClient = \case
            Nothing -> Nothing
            Just c
              | c.id == client.id -> Just Nothing
              | otherwise -> Nothing
      for_ (IntMap.alterF deleteIfSameClient session.id clients) $ STM.writeTVar table.clients
    run client =
      let send = undefined
          receive = STM.takeTMVar client.messageBox
       in handle send receive

data TableStatus
  = Waiting Int
  | Playing
  | Done
  deriving (Eq, Show)

newtype TableState = TableState
  { players :: Vector4 (Maybe SessionId)
  }
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

data JoinError = TableIsFull
  deriving (Show)

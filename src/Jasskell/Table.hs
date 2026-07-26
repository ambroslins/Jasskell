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
import Jasskell.GameState (DeclareError, GameState, ShoveError, UnplayableCardReason)
import Jasskell.Id (Id (..))
import Jasskell.Id qualified as Id
import Jasskell.Player (Player (..), PlayerId)
import Jasskell.Variant (Variant)
import UnliftIO (MonadUnliftIO, bracket)
import Prelude hiding (lookup)

type TableId = Id Table

newtype TableManager = TableManager
  { tables :: IORef (IntMap TableId Table)
  }

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
    player :: Player,
    messageBox :: STM.TMVar ServerMessage
  }

newtype ServerMessage = ServerMessage TableState
  deriving (Show)

data ClientMessage

data Event
  = PlayerJoined PlayerId
  | PlayerLeft PlayerId
  | ClientMessage
  deriving (Eq, Show)

data Table = Table
  { id :: TableId,
    eventQueue :: STM.TBQueue Event,
    clients :: STM.TVar (IntMap PlayerId Client),
    async :: Async ()
  }

new :: (MonadIO m) => TableManager -> m TableId
new tm = liftIO $ do
  tableId <- Id.new
  eventQueue <- STM.newTBQueueIO 16
  clients <- STM.newTVarIO IntMap.empty
  async <- Async.async $ tableLoop eventQueue clients
  let table = Table {id = tableId, eventQueue, clients, async}
  atomicModifyIORef' tm.tables ((,()) . IntMap.insert tableId table)
  pure tableId

tableLoop :: STM.TBQueue Event -> STM.TVar (IntMap PlayerId Client) -> IO ()
tableLoop eventQueue clientsVar = go inital
  where
    inital = Waiting $ Vector4.replicate Nothing
    _broadcast msg = do
      clients <- STM.atomically $ STM.readTVar clientsVar
      IntMap.forWithKey_ clients $ \_userId client ->
        STM.atomically $ STM.writeTMVar client.messageBox msg
    go state = do
      event <- STM.atomically $ STM.readTBQueue eventQueue
      putStrLn $ "got event" <> show event
      go state

withClient :: (MonadUnliftIO m) => Table -> Player -> ((ClientMessage -> STM ()) -> STM ServerMessage -> m a) -> m a
withClient table player handle = bracket enter leave run
  where
    enter = liftIO $ do
      clientId <- Id.new
      messageBox <- STM.newEmptyTMVarIO
      let client = Client {id = clientId, player, messageBox}
      STM.atomically $ do
        -- TODO: notify old client if necessary
        STM.modifyTVar' table.clients (IntMap.insert player.id client)
        STM.writeTBQueue table.eventQueue (PlayerJoined player.id)
      pure client
    leave client = liftIO $ STM.atomically $ do
      clients <- STM.readTVar table.clients
      let deleteIfSameClient = \case
            Nothing -> Nothing
            Just c
              | c.id == client.id -> Just Nothing
              | otherwise -> Nothing
      for_ (IntMap.alterF deleteIfSameClient player.id clients) $ STM.writeTVar table.clients
    run client =
      let send = undefined
          receive = STM.takeTMVar client.messageBox
       in handle send receive

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

data JoinError = TableIsFull
  deriving (Show)

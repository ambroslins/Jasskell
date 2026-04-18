module Jasskell.Table
  ( TableManager,
    withManager,
    Table,
    new,
    lookup,
    withEntry,
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
import Control.Exception (bracket)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Coercible (IntMap)
import Data.IntMap.Coercible qualified as IntMap
import Data.Vector4 (Vector4)
import Jasskell.Card (Card)
import Jasskell.GameState (DeclareError, GameState, ShoveError, UnplayableCardReason)
import Jasskell.Id (Id (..))
import Jasskell.Id qualified as Id
import Jasskell.User (User (..), UserId)
import Jasskell.Variant (Variant)
import Prelude hiding (lookup)

type TableId = Id Table

newtype TableManager = TableManager {tables :: IORef (IntMap TableId Table)}

withManager :: (TableManager -> IO a) -> IO a
withManager f = do
  tm <- TableManager <$> newIORef IntMap.empty
  -- TODO: reaper thread
  -- TODO: stop all tables on cleanup
  f tm

lookup :: TableId -> TableManager -> IO (Maybe Table)
lookup tableId tm = IntMap.lookup tableId <$> readIORef tm.tables

type ClientId = Id Client

data Client = Client
  { id :: ClientId,
    user :: User,
    messageBox :: STM.TMVar ServerMessage
  }

data ServerMessage

data ClientMessage

data Event
  = UserJoined UserId
  | UserLeft UserId
  | ClientMessage
  deriving (Eq, Show)

data Table = Table
  { id :: TableId,
    eventQueue :: STM.TBQueue Event,
    clients :: STM.TVar (IntMap UserId Client),
    status :: STM.TVar TableStatus,
    async :: Async ()
  }

new :: TableManager -> IO TableId
new tm = do
  tableId <- Id.new
  eventQueue <- STM.newTBQueueIO 16
  clients <- STM.newTVarIO IntMap.empty
  status <- STM.newTVarIO $ Waiting 0
  async <- Async.async $ pure () -- TODO: table loop
  let table = Table {id = tableId, eventQueue, clients, status, async}
  atomicModifyIORef' tm.tables ((,()) . IntMap.insert tableId table)
  pure tableId

withEntry :: Table -> User -> ((ClientMessage -> STM ()) -> STM ServerMessage -> IO a) -> IO a
withEntry table user handle = bracket enter leave run
  where
    enter = do
      clientId <- Id.new
      messageBox <- STM.newEmptyTMVarIO
      let client = Client {id = clientId, user, messageBox}
      STM.atomically $ do
        -- TODO: notify old client if necessary
        STM.modifyTVar' table.clients (IntMap.insert user.id client)
      pure client
    leave client = STM.atomically $ do
      clients <- STM.readTVar table.clients
      let deleteIfSameClient = \case
            Nothing -> Nothing
            Just c
              | c.id == client.id -> Just Nothing
              | otherwise -> Nothing
      for_ (IntMap.alterF deleteIfSameClient user.id clients) $ STM.writeTVar table.clients
    run client =
      let send = undefined
          receive = STM.takeTMVar client.messageBox
       in handle send receive

data TableStatus
  = Waiting Int
  | Playing
  | Done
  deriving (Eq, Show)

data TableState = TableState
  { players :: Vector4 (Maybe UserId),
    gameState :: GameState
  }

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

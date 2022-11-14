module Jasskell.Server.Table
  ( Table,
    SomeTable (..),
    new,
    join,
  )
where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Jasskell.Dealer (Dealer)
import Jasskell.Jass (JassNat)
import Jasskell.Server.Action (Action)
import Jasskell.Server.Client (Client)
import Jasskell.Server.Client qualified as Client
import Jasskell.Server.Message (Message)
import Jasskell.Server.Message qualified as Message
import Jasskell.Server.TableState (TableState)
import Jasskell.Server.TableState qualified as TableState
import Prelude hiding (join)

data Event n
  = ClientAction (Client n) (Action n)
  | ClientJoined (Client n)
  deriving (Eq)

data Table n = MakeTable
  { thread :: Async (),
    eventQueue :: TBQueue (Event n)
  }

data SomeTable = forall n. JassNat n => SomeTable (Table n)

join :: MonadIO m => (Message n -> IO ()) -> Table n -> m (Action n -> IO ())
join sendMessage table = do
  (client, sendAction) <- Client.make sendMessage writeClientAction
  atomically $ writeTBQueue (eventQueue table) (ClientJoined client)
  pure sendAction
  where
    writeClientAction client action =
      writeTBQueue queue (ClientAction client action)
    queue = eventQueue table

new :: (JassNat n, MonadIO m) => Dealer n -> m (Table n)
new dealer = do
  tableState <- TableState.make dealer
  queue <- liftIO (newTBQueueIO 8)
  a <- liftIO $ async (run (readTBQueue queue) tableState)
  pure
    MakeTable
      { thread = a,
        eventQueue = queue
      }

run :: (JassNat n, MonadIO m) => STM (Event n) -> TableState n -> m ()
run waitEvent = fix $ \loop tableState -> do
  event <- atomically waitEvent
  case event of
    ClientAction client action ->
      case TableState.applyAction client action tableState of
        Left e -> do
          Client.sendMessage client (Message.Error e)
          loop tableState
        Right ts -> do
          TableState.broadcast ts
          loop ts
    ClientJoined client -> do
      let ts = TableState.addGuest client tableState
      TableState.broadcast ts
      loop ts

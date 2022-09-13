module Jasskell.Server.Table
  ( Table,
    SomeTable (..),
    new,
    join,
  )
where

import Control.Concurrent.STM.TVar (modifyTVar)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector.Sized qualified as Vector
import Jasskell.Dealer (Dealer)
import Jasskell.Jass (JassNat)
import Jasskell.Server.Action (Action)
import Jasskell.Server.Action qualified as Action
import Jasskell.Server.Client (ClientID)
import Jasskell.Server.Client qualified as Client
import Jasskell.Server.Message (Message)
import Jasskell.Server.Message qualified as Message
import Jasskell.Server.TableID (TableID)
import Jasskell.Server.TableID qualified as TableID
import Jasskell.Server.TableState
  ( Phase (Waiting),
    Seat (..),
    TableState (..),
    findClient,
    viewGuest,
    viewPlayer,
  )
import System.Random (newStdGen)
import Prelude hiding (join)

newtype Table n = Table (TVar (TableState n))

data SomeTable = forall n. JassNat n => SomeTable (Table n)

new :: (KnownNat n, MonadIO m) => Dealer n -> m (TableID, Table n)
new d = do
  gen <- newStdGen
  let tableState =
        TableState
          { guests = HashMap.empty,
            seats = Vector.replicate Empty,
            stdGen = gen,
            dealer = d,
            phase = Waiting
          }
  table <- Table <$> newTVarIO tableState
  tableID <- TableID.new
  pure (tableID, table)

join :: (JassNat n, MonadIO m) => Table n -> m (Action n -> IO (), IO (Message n))
join (Table table) = do
  (clientID, client) <- Client.make
  atomically $ do
    modifyTVar table $ \tableState ->
      tableState
        { guests = HashMap.insert clientID client $ guests tableState
        }
    broadcastView table
  pure
    ( atomically . runAction table clientID,
      atomically $ Client.recive client
    )

runAction :: forall n. JassNat n => TVar (TableState n) -> ClientID -> Action n -> STM ()
runAction table clientID action = do
  tableState <- readTVar table
  case findClient clientID tableState of
    Nothing -> pure ()
    Just (client, mplayer) ->
      case Action.run action clientID client mplayer tableState of
        Left e -> Client.send client $ Message.Error e
        Right ts -> do
          writeTVar table ts
          broadcastView table

broadcastView :: KnownNat n => TVar (TableState n) -> STM ()
broadcastView table = do
  tableState <- readTVar table
  let sendPlayer i = \case
        Empty -> pure ()
        Taken _ _ client ->
          Client.send client $
            Message.UpdatePlayerView (viewPlayer tableState i)
      sendGuest client =
        Client.send client $
          Message.UpdateGuestView (viewGuest tableState)
  Vector.imapM_ sendPlayer $ seats tableState
  mapM_ sendGuest $ guests tableState

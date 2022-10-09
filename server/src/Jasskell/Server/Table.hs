module Jasskell.Server.Table
  ( Table,
    SomeTable (..),
    new,
    join,
  )
where

import Control.Concurrent.STM.TVar (modifyTVar)
import Data.HashSet qualified as HashSet
import Data.Vector.Sized qualified as Vector
import Jasskell.Dealer (Dealer)
import Jasskell.Jass (JassNat)
import Jasskell.Server.Action (Action)
import Jasskell.Server.Action qualified as Action
import Jasskell.Server.Client (Client)
import Jasskell.Server.Client qualified as Client
import Jasskell.Server.Message (Message)
import Jasskell.Server.Message qualified as Message
import Jasskell.Server.Player qualified as Player
import Jasskell.Server.TableState
  ( Phase (Waiting),
    TableState (..),
    guestView,
    playerViews,
  )
import Jasskell.Views qualified as Views
import System.Random (newStdGen)
import Prelude hiding (join)

newtype Table n = Table (TVar (TableState n))

data SomeTable = forall n. JassNat n => SomeTable (Table n)

new :: (KnownNat n, MonadIO m) => Dealer n -> m (Table n)
new d = do
  gen <- newStdGen
  let tableState =
        TableState
          { guests = HashSet.empty,
            players = Vector.replicate Nothing,
            stdGen = gen,
            dealer = d,
            phase = Waiting
          }
  Table <$> newTVarIO tableState

join :: (JassNat n, MonadIO m) => Table n -> m (Action n -> IO (), IO (Message n))
join (Table table) = do
  client <- Client.make
  atomically $ do
    modifyTVar table $ \ts -> ts {guests = HashSet.insert client $ guests ts}
    broadcastView table
  pure
    ( atomically . runAction table client,
      atomically $ Client.recive client
    )

runAction :: JassNat n => TVar (TableState n) -> Client n -> Action n -> STM ()
runAction table client action = do
  tableState <- readTVar table
  case Action.run action client tableState of
    Left e -> Client.send client $ Message.Error e
    Right ts -> do
      writeTVar table ts
      broadcastView table

broadcastView :: KnownNat n => TVar (TableState n) -> STM ()
broadcastView table = do
  tableState <- readTVar table
  let sendPlayer i =
        maybe
          pass
          ( \p ->
              Client.send
                (Player.client p)
                (Message.UpdatePlayerView $ Views.pov (playerViews tableState) i)
          )
      sendGuest c =
        Client.send
          c
          (Message.UpdateGuestView (guestView tableState))
  Vector.imapM_ sendPlayer (players tableState)
  traverse_ sendGuest (guests tableState)

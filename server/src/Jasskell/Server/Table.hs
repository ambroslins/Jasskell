module Jasskell.Server.Table where

import Control.Concurrent.STM (writeTMVar)
import Control.Monad.Except (MonadError (..))
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Game (Game)
import Jasskell.Server.Action (Action (..))
import Jasskell.Server.Error (Error (..))
import Jasskell.Server.GameState (GameState, Transition (..))
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.Message (Message (..))
import Jasskell.Server.TableID (TableID)
import Jasskell.Server.TableID qualified as TableID
import Jasskell.Server.User (User)
import Prelude hiding (join)

newtype Table n = Table (TVar (TableState n))

data TableState n = TableState
  { seats :: Vector n (Seat n),
    phase :: Phase n
  }

data Seat n
  = Empty
  | Taken User (Message n -> STM ())

data Phase n
  = Waiting
  | Playing (GameState n)
  | Over (Game n)

new :: KnownNat n => IO (TableID, Table n)
new = do
  let tableState =
        TableState
          { seats = Vector.replicate Empty,
            phase = Waiting
          }
  table <- Table <$> newTVarIO tableState
  tableID <- TableID.new
  pure (tableID, table)

data Connection n = Connection
  { putAction :: Action -> STM (Either Error ()),
    getMessage :: STM (Message n)
  }

join :: User -> Finite n -> Table n -> STM (Maybe (Connection n))
join user index t@(Table table) = do
  messageVar <- newEmptyTMVar
  tableState <- readTVar table
  let sit seat = guard (isEmpty seat) $> Taken user (writeTMVar messageVar)
  case Vector.ix index sit $ seats tableState of
    Nothing -> pure Nothing
    Just s ->
      writeTVar table tableState {seats = s}
        $> pure
          Connection
            { putAction = applyAction t index,
              getMessage = takeTMVar messageVar
            }

applyAction :: forall n. Table n -> Finite n -> Action -> STM (Either Error ())
applyAction (Table table) player action = do
  tableState <- readTVar table
  case updateState tableState of
    Left e -> pure $ Left e
    Right ts -> pure <$> writeTVar table ts
  where
    updateState :: TableState n -> Either Error (TableState n)
    updateState ts = case action of
      Move move -> case phase ts of
        Waiting -> throwError WaitingForPlayers
        Over _ -> throwError GameOver
        Playing gameState -> do
          p <- case GameState.update player move gameState of
            Left badMove -> throwError $ BadMove badMove
            Right (Continue gs) -> pure $ Playing gs
            Right (Done game) -> pure $ Over game
          pure $ ts {phase = p}

isEmpty :: Seat n -> Bool
isEmpty = \case
  Empty -> True
  _ -> False

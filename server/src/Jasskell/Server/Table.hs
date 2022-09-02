module Jasskell.Server.Table
  ( Table,
    new,
    join,
    Connection,
    getView,
    putAction,
  )
where

import Control.Concurrent.STM (writeTMVar)
import Control.Monad.Except (MonadError (..))
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import Jasskell.Dealer (Dealer)
import Jasskell.Game (Game)
import Jasskell.Jass (JassNat)
import Jasskell.Server.Action (Action (..))
import Jasskell.Server.Error (Error (..))
import Jasskell.Server.GameState (BadMove, GameState, Transition (..))
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.TableID (TableID)
import Jasskell.Server.TableID qualified as TableID
import Jasskell.Server.User (User)
import Jasskell.Server.View (View)
import Jasskell.Server.View qualified as View
import Jasskell.Views qualified as Views
import System.Random (StdGen, newStdGen, split)
import Prelude hiding (join)
import qualified Jasskell.Game as Game

newtype Table n = Table (TVar (TableState n))

data TableState n = TableState
  { seats :: Vector n (Seat n),
    stdGen :: StdGen,
    dealer :: Dealer n,
    phase :: Phase n
  }

data Seat n
  = Empty
  | Taken User (View n -> STM ())

data Phase n
  = Waiting
  | Playing (GameState n)
  | Over (Game n)

new :: KnownNat n => Dealer n -> IO (TableID, Table n)
new d = do
  gen <- newStdGen
  let tableState =
        TableState
          { seats = Vector.replicate Empty,
            stdGen = gen,
            dealer = d,
            phase = Waiting
          }
  table <- Table <$> newTVarIO tableState
  tableID <- TableID.new
  pure (tableID, table)

data Connection n = Connection
  { putAction :: Action -> STM (Either Error ()),
    getView :: STM (View n)
  }

join :: JassNat n => User -> Finite n -> Table n -> STM (Maybe (Connection n))
join user index t@(Table table) = do
  viewVar <- newEmptyTMVar
  tableState <- readTVar table
  let sit seat = guard (isEmpty seat) $> Taken user (writeTMVar viewVar)
  case Vector.ix index sit $ seats tableState of
    Nothing -> pure Nothing
    Just s -> do
      writeTableState table tableState {seats = s}
        $> pure
          Connection
            { putAction = applyAction t index,
              getView = takeTMVar viewVar
            }

applyAction ::
  forall n.
  JassNat n =>
  Table n ->
  Finite n ->
  Action ->
  STM (Either Error ())
applyAction (Table table) player action = do
  tableState <- readTVar table
  case updateState tableState of
    Left e -> pure $ Left e
    Right ts -> pure <$> writeTableState table ts
  where
    updateState :: TableState n -> Either Error (TableState n)
    updateState ts = case action of
      Move move -> case phase ts of
        Waiting -> throwError WaitingForPlayers
        Over _ -> throwError GameOver
        Playing gameState -> do
          p <- fromTransition $ GameState.update player move gameState
          pure $ ts {phase = p}
      StartGame -> case phase ts of
        Playing _ -> throwError GameAlreadyStarted
        _ -> do
          let (g, g') = split $ stdGen ts
          p <- fromTransition $ GameState.start (dealer ts) g
          pure $ ts {phase = p, stdGen = g'}

    fromTransition :: Either BadMove (Transition n) -> Either Error (Phase n)
    fromTransition =
      either (throwError . BadMove) $
        pure . \case
          Continue gameState -> Playing gameState
          Done game -> Over game

writeTableState :: KnownNat n => TVar (TableState n) -> TableState n -> STM ()
writeTableState tvar tableState = do
  writeTVar tvar tableState
  broadcastView tableState

broadcastView :: KnownNat n => TableState n -> STM ()
broadcastView tableState = Vector.imapM_ send $ seats tableState
  where
    views = view tableState
    send i = \case
      Empty -> pure ()
      Taken _ sendView -> sendView (views i)

isEmpty :: Seat n -> Bool
isEmpty = \case
  Empty -> True
  _ -> False

view :: KnownNat n => TableState n -> Finite n -> View n
view ts player =
  View.MakeView
    { View.seats =
        Vector.rotate (negate player) $ Vector.map mapSeat $ seats ts,
      View.phase = case phase ts of
        Waiting -> View.Waiting
        Playing gameState ->
          View.Playing $ Views.pov (GameState.views gameState) player
        Over game -> View.Over $ Game.rotate player game
    }
  where
    mapSeat :: Seat n -> View.Seat
    mapSeat = \case
      Empty -> View.Empty
      Taken user _ -> View.Taken user

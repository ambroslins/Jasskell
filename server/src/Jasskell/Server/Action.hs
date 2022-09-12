module Jasskell.Server.Action
  ( Action,
    run,
    takeSeat,
    playMove,
    startGame,
  )
where

import Control.Monad.Except (throwError)
import Data.Finite (Finite)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector.Sized qualified as Vector
import Jasskell.Jass (JassNat)
import Jasskell.Server.Client (Client, ClientID)
import Jasskell.Server.Error (Error (..))
import Jasskell.Server.GameState (BadMove, Move, Transition (..))
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.TableState
  ( Phase (..),
    Seat (Taken),
    TableState (..),
    isEmpty,
  )
import Jasskell.Server.User (User (User))
import System.Random (RandomGen (split))

newtype Action n = Action
  { run ::
      ClientID ->
      Client n ->
      Maybe (Finite n) ->
      TableState n ->
      Either Error (TableState n)
  }

takeSeat :: Text -> Finite n -> Action n
takeSeat username index = Action $ \clientID client mplayer tableState -> do
  when (isJust mplayer) $ throwError AlreadySeated
  let sit s = guard (isEmpty s) $> Taken (User username) clientID client
  case Vector.ix index sit $ seats tableState of
    Nothing -> throwError SeatAlreadyTaken
    Just s ->
      pure $
        tableState
          { seats = s,
            guests = HashMap.delete clientID $ guests tableState
          }

playMove :: Move -> Action n
playMove move = Action $ \_ _ mplayer tableState -> do
  case phase tableState of
    Waiting -> throwError WaitingForPlayers
    Over _ -> throwError GameOver
    Playing gameState -> do
      player <- maybe (throwError NotAPlayer) pure mplayer
      let transition = GameState.update player move gameState
      p <- fromTransition transition
      pure $ tableState {phase = p}

startGame :: JassNat n => Action n
startGame = Action $ \_ _ _ tableState -> do
  case phase tableState of
    Playing _ -> throwError GameAlreadyStarted
    _ -> do
      let (g, g') = split $ stdGen tableState
      p <- fromTransition $ GameState.start (dealer tableState) g
      pure $
        tableState
          { phase = p,
            stdGen = g'
          }

fromTransition :: Either BadMove (Transition n) -> Either Error (Phase n)
fromTransition =
  either (throwError . BadMove) $
    pure . \case
      Continue gameState -> Playing gameState
      Done game -> Over game

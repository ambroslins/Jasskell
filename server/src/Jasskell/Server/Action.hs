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
import Data.HashSet qualified as HashSet
import Data.Vector.Sized qualified as Vector
import Jasskell.Jass (JassNat)
import Jasskell.Server.Client (Client)
import Jasskell.Server.Error (Error (..))
import Jasskell.Server.GameState (BadMove, Move, Transition (..))
import Jasskell.Server.GameState qualified as GameState
import Jasskell.Server.TableState
  ( Phase (..),
    Seat (Taken),
    TableState (..),
    findPlayer,
    isEmptySeat,
  )
import Jasskell.Server.User (User (User))
import System.Random (RandomGen (split))

newtype Action n = Action
  {run :: Client n -> TableState n -> Either Error (TableState n)}

takeSeat :: Text -> Finite n -> Action n
takeSeat username index = Action $ \client tableState -> do
  when (isJust $ findPlayer client tableState) $ throwError AlreadySeated
  let sit s = guard (isEmptySeat s) $> Taken (User username) client
  case Vector.ix index sit $ seats tableState of
    Nothing -> throwError SeatAlreadyTaken
    Just ss ->
      pure $
        tableState
          { seats = ss,
            guests = HashSet.delete client $ guests tableState
          }

playMove :: Move -> Action n
playMove move = Action $ \client tableState -> do
  case phase tableState of
    Waiting -> throwError WaitingForPlayers
    Over _ -> throwError GameOver
    Playing gameState -> do
      player <-
        whenNothing
          (findPlayer client tableState)
          (throwError NotAPlayer)
      p <- fromTransition (GameState.update player move gameState)
      pure $ tableState {phase = p}

startGame :: JassNat n => Action n
startGame = Action $ \_ tableState -> do
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
fromTransition = bimap BadMove $ \case
  Continue gameState -> Playing gameState
  Done game -> Over game

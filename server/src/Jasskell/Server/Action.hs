module Jasskell.Server.Action where

{-( Action,
  run,
  takeSeat,
  playMove,
  startGame,
)-}

import Data.Finite (Finite)
import Jasskell.Server.GameState (Move)

data Action n
  = TakeSeat Text (Finite n)
  | PlayMove Move
  | StartGame
  deriving (Eq, Show)

{-
newtype Action n = Action
  {run :: Client n -> TableState n -> Either Error (TableState n)}

takeSeat :: Text -> Finite n -> Action n
takeSeat username index = Action $ \client tableState -> do
  when (isJust $ findPlayer client tableState) (throwError AlreadySeated)
  let sit s = guard (isNothing s) $> Just (Player.make (User username) client)
  case Vector.ix index sit (players tableState) of
    Nothing -> throwError SeatAlreadyTaken
    Just ps ->
      pure $
        tableState
          { players = ps,
            guests = HashSet.delete client (guests tableState)
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
      pure (tableState {phase = p})

startGame :: JassNat n => Action n
startGame = Action $ \_ tableState -> do
  case phase tableState of
    Playing _ -> throwError GameAlreadyStarted
    _ -> do
      let (g, g') = split (stdGen tableState)
      p <- fromTransition $ GameState.start (dealer tableState) g
      pure (tableState {phase = p, stdGen = g'})

fromTransition :: Either BadMove (Transition n) -> Either Error (Phase n)
fromTransition = bimap BadMove $ \case
  Continue gameState -> Playing gameState
  Done game -> Over game
  -}

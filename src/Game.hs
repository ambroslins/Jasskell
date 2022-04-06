module Game where

import Control.Monad.State (evalStateT, state)
import Jass (JassNat (..), MonadJass)
import Round (Round)
import Round qualified
import System.Random (RandomGen)

newtype Game n = Game
  { rounds :: NonEmpty (Round n)
  }
  deriving (Show)

play :: (MonadJass n m, RandomGen g) => g -> m (Game n)
play = evalStateT $ do
  decks <- state deal
  firstRound <- Round.play decks 0
  pure (Game (firstRound :| []))

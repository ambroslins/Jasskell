module Jasskell.Game where

import Jasskell.Jass (MonadJass, deal)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import System.Random (RandomGen)

newtype Game n = Game {rounds :: NonEmpty (Round n)}
  deriving (Show)

play :: (MonadJass n m, RandomGen g) => g -> m (Game n)
play = evalStateT $ do
  hands <- state deal
  r <- Round.play 0 hands
  pure $ Game (r :| [])

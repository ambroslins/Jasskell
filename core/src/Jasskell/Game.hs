module Jasskell.Game where

import Control.Monad.Free (MonadFree)
import Jasskell.Game.View (View)
import Jasskell.Game.View qualified as View
import Jasskell.Jass (JassNat, deal)
import Jasskell.Prompt (Prompt)
import Jasskell.Prompt qualified as Prompt
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import System.Random (RandomGen)

newtype Game n = Game {rounds :: [Round n]}
  deriving (Show)

play ::
  (JassNat n, MonadFree (Prompt (View n)) m, RandomGen g) =>
  g ->
  m (Game n)
play = evalStateT $ go [] 0
  where
    go rs leader =
      -- TODO: check if one team has reached the point limit
      if length rs >= 10
        then pure $ Game rs
        else do
          hands <- state deal
          let adapt = Prompt.mapView (View.fromRound rs)
          r <- adapt $ Round.play leader hands
          go (r : rs) (leader + 1)

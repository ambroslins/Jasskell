module Jasskell.Game where

import Control.Monad.Free (MonadFree)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (Cards, Suit (Bells))
import Jasskell.Game.View (View)
import Jasskell.Game.View qualified as View
import Jasskell.Jass (JassNat, deal)
import Jasskell.Prompt (Prompt)
import Jasskell.Prompt qualified as Prompt
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Variant (Variant (Trump))
import System.Random (RandomGen)

newtype Game n = Game {rounds :: [Round n]}
  deriving (Show)

play ::
  forall n m g.
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
          r <- lift $ playRound hands leader rs
          go (r : rs) (leader + 1)

    playRound :: Vector n Cards -> Finite n -> [Round n] -> m (Round n)
    playRound hands leader rounds = do
      let variant = Trump Bells -- TODO: let the user choose
          prompt view _ =
            Prompt.card $
              View.MakeView
                { View.previousRounds = rounds,
                  View.current = View.Playing view
                }
      Round.play prompt variant leader hands

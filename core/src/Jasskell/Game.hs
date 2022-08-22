module Jasskell.Game where

import Control.Monad.Except (MonadError)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (Cards, Suit (Bells))
import Jasskell.Card qualified as Card
import Jasskell.Interface (Interface)
import Jasskell.Interface qualified as Interface
import Jasskell.Jass (JassNat, deal)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Variant (Variant (Trump))
import Jasskell.View.Playing qualified as View.Playing
import System.Random (RandomGen)

newtype Game n = Game {rounds :: [Round n]}
  deriving (Show)

play ::
  forall n m g.
  (JassNat n, MonadError Card.Reason m, RandomGen g) =>
  Interface n m ->
  g ->
  m (Game n)
play interface = evalStateT $ go [] 0
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
          prompt current view =
            Interface.promptCard interface current $
              View.Playing.make rounds view
      Round.play prompt variant leader hands

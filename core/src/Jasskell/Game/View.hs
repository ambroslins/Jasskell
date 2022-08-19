module Jasskell.Game.View where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (Card, Cards)
import Jasskell.Round (Round)
import Jasskell.Round.View qualified as Round.View
import Jasskell.Trick (Trick)
import Jasskell.Variant (Variant)

data View n = MakeView
  { hands :: Vector n Cards,
    table :: Vector n (Maybe Card),
    leader :: Finite n,
    variant :: Maybe Variant,
    tricks :: [Trick n],
    rounds :: [Round n]
  }
  deriving (Eq, Show)

fromRound :: [Round n] -> Round.View.View n -> View n
fromRound rs v =
  MakeView
    { hands = Round.View.hands v,
      table = Round.View.table v,
      leader = Round.View.leader v,
      variant = Round.View.variant v,
      tricks = Round.View.tricks v,
      rounds = rs
    }

module Jasskell.Round.View where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (Card, Cards)
import Jasskell.Trick (Trick)
import Jasskell.Variant (Variant)

data View n = MakeView
  { hands :: Vector n Cards,
    tricks :: [Trick n],
    variant :: Variant,
    leader :: Finite n,
    cards :: [Card]
  }
  deriving (Eq, Show)

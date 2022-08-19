module Jasskell.Trick.View (View (..)) where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (Card, Cards)
import Jasskell.Variant (Variant)

data View n = MakeView
  { hands :: Vector n Cards,
    table :: Vector n (Maybe Card),
    leader :: Finite n,
    variant :: Variant
  }
  deriving (Eq, Show)

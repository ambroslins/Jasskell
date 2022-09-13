module Jasskell.Trick.State (TrickState (..)) where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (Card, Cards)
import Jasskell.Variant (Variant)

data TrickState n = TrickState
  { hands :: Vector n Cards,
    variant :: Variant,
    leader :: Finite n,
    cards :: [Card]
  }
  deriving (Eq, Show)

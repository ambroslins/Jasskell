module Jasskell.View.Absolute
  ( View (..),
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (Card, Cards)
import Jasskell.Variant (Variant)

data View n = MakeView
  { hands :: Vector n Cards,
    cards :: Vector n (Maybe Card),
    variant :: Maybe Variant,
    leader :: Finite n
  }
  deriving (Eq, Show)

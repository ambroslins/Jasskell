module Jasskell.View.Relative
  ( View,
    fromAbsolute,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import Jasskell.Card (Card, Cards)
import Jasskell.Variant (Variant)
import Jasskell.View.Absolute qualified as Absolute

data View n = MakeView
  { hand :: Cards,
    cards :: Vector n (Maybe Card),
    variant :: Maybe Variant,
    leader :: Finite n
  }
  deriving (Eq, Show)

fromAbsolute :: KnownNat n => Absolute.View n -> Finite n -> View n
fromAbsolute view i =
  MakeView
    { hand = Vector.index (Absolute.hands view) i,
      cards = Vector.rotate i (Absolute.cards view),
      variant = Absolute.variant view,
      leader = Absolute.leader view - i
    }
module Jasskell.Round.View
  ( View (..),
    fromTrick,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (Card, Cards)
import Jasskell.Trick (Trick)
import Jasskell.Trick.View qualified as Trick.View
import Jasskell.Variant (Variant)

data View n = MakeView
  { hands :: Vector n Cards,
    table :: Vector n (Maybe Card),
    leader :: Finite n,
    variant :: Maybe Variant,
    tricks :: [Trick n]
  }
  deriving (Eq, Show)

fromTrick :: [Trick n] -> Trick.View.View n -> View n
fromTrick ts v =
  MakeView
    { hands = Trick.View.hands v,
      table = Trick.View.table v,
      leader = Trick.View.leader v,
      variant = Just $ Trick.View.variant v,
      tricks = ts
    }

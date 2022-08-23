module Jasskell.Round.View
  ( View,
    fromTrick,
    hands,
    tricks,
    variant,
    leader,
    cards,
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
    tricks :: [Trick n],
    variant :: Variant,
    leader :: Finite n,
    cards :: [Card]
  }
  deriving (Eq, Show)

fromTrick :: [Trick n] -> Trick.View.View n -> View n
fromTrick tricks view =
  MakeView
    { hands = Trick.View.hands view,
      tricks = tricks,
      variant = Trick.View.variant view,
      leader = Trick.View.leader view,
      cards = Trick.View.cards view
    }
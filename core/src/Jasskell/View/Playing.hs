module Jasskell.View.Playing
  ( Playing,
    hand,
    rounds,
    tricks,
    variant,
    leader,
    table,
    make,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import Jasskell.Card (Card, Cards)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Round.View qualified as Round.View
import Jasskell.Trick (Trick)
import Jasskell.Trick qualified as Trick
import Jasskell.Variant (Variant)
import Jasskell.Views (Views)
import Jasskell.Views qualified as Views

data Playing n = Playing
  { hand :: Cards,
    rounds :: [Round n],
    tricks :: [Trick n],
    variant :: Variant,
    leader :: Finite n,
    cards :: [Card]
  }
  deriving (Eq, Show)

make :: KnownNat n => [Round n] -> Round.View.View n -> Views Playing n
make rs view = Views.make $ \player ->
  Playing
    { hand = Vector.index (Round.View.hands view) player,
      rounds = map (Round.rotate player) rs,
      tricks = map (Trick.rotate player) $ Round.View.tricks view,
      variant = Round.View.variant view,
      leader = Round.View.leader view - player,
      cards = Round.View.cards view
    }

table :: KnownNat n => Playing n -> Vector n (Maybe Card)
table Playing {leader, cards} =
  Vector.rotate (negate leader) $
    Vector.unfoldrN
      (maybe (Nothing, []) (first Just) . uncons)
      cards

module Jasskell.Trick.View
  ( TrickView,
    makeViews,
    hand,
    variant,
    leader,
    playedCards,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import Jasskell.Card (Card, Cards)
import Jasskell.Variant (Variant)
import Jasskell.Views (Views)
import Jasskell.Views qualified as Views

data TrickView n = MakeTrickView
  { hand :: Cards,
    variant :: Variant,
    leader :: Finite n,
    cards :: [Card]
  }
  deriving (Eq, Show)

playedCards :: KnownNat n => TrickView n -> Vector n (Maybe Card)
playedCards trickView =
  Vector.rotate (leader trickView) $
    Vector.unfoldrN
      (maybe (Nothing, []) (first Just) . uncons)
      (cards trickView)

makeViews ::
  KnownNat n =>
  Vector n Cards ->
  Variant ->
  Finite n ->
  [Card] ->
  Views TrickView n
makeViews hands var l cs = Views.make $ \i ->
  MakeTrickView
    { hand = Vector.index hands i,
      variant = var,
      leader = l - i,
      cards = cs
    }
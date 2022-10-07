module Jasskell.Round.View
  ( RoundView,
    makeViews,
    hand,
    variant,
    leader,
    playedCards,
    tricks,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (Card, Cards)
import Jasskell.Trick (Trick)
import Jasskell.Trick qualified as Trick
import Jasskell.Trick.View (TrickView)
import Jasskell.Trick.View qualified as Trick.View
import Jasskell.Variant (Variant)
import Jasskell.Views (Views)
import Jasskell.Views qualified as Views

data RoundView n = MakeRoundView
  { tricks :: [Trick n],
    current :: TrickView n
  }
  deriving (Eq, Show)

makeViews :: KnownNat n => [Trick n] -> Views TrickView n -> Views RoundView n
makeViews ts = Views.map $ \i trickView ->
  MakeRoundView
    { tricks = map (Trick.rotate i) ts,
      current = trickView
    }

hand :: RoundView n -> Cards
hand = Trick.View.hand . current

variant :: RoundView n -> Variant
variant = Trick.View.variant . current

leader :: RoundView n -> Finite n
leader = Trick.View.leader . current

playedCards :: KnownNat n => RoundView n -> Vector n (Maybe Card)
playedCards = Trick.View.playedCards . current

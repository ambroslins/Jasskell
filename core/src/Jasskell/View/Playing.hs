module Jasskell.View.Playing
  ( ViewPlaying,
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
import Jasskell.Round.State (RoundState)
import Jasskell.Round.State qualified as Round.State
import Jasskell.Trick (Trick)
import Jasskell.Trick qualified as Trick
import Jasskell.Variant (Variant)
import Jasskell.Views (Views)
import Jasskell.Views qualified as Views

data ViewPlaying n = ViewPlaying
  { hand :: Cards,
    rounds :: [Round n],
    tricks :: [Trick n],
    variant :: Variant,
    leader :: Finite n,
    cards :: [Card]
  }
  deriving (Eq, Show)

make :: KnownNat n => [Round n] -> RoundState n -> Views ViewPlaying n
make rs view = Views.make $ \player ->
  ViewPlaying
    { hand = Vector.index (Round.State.hands view) player,
      rounds = map (Round.rotate player) rs,
      tricks = map (Trick.rotate player) $ Round.State.tricks view,
      variant = Round.State.variant view,
      leader = Round.State.leader view - player,
      cards = Round.State.cards view
    }

table :: KnownNat n => ViewPlaying n -> Vector n (Maybe Card)
table ViewPlaying {leader, cards} =
  Vector.rotate (negate leader) $
    Vector.unfoldrN
      (maybe (Nothing, []) (first Just) . uncons)
      cards

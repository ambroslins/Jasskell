module Jasskell.View.Playing
  ( ViewPlaying,
    makeViews,
    hand,
    variant,
    leader,
    playedCards,
    tricks,
    rounds,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (Card, Cards)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Round.View (RoundView)
import Jasskell.Round.View qualified as Round.View
import Jasskell.Trick (Trick)
import Jasskell.Variant (Variant)
import Jasskell.Views (Views)
import Jasskell.Views qualified as Views

data ViewPlaying n = MakeViewPlaying
  { rounds :: [Round n],
    current :: RoundView n
  }
  deriving (Eq, Show)

makeViews :: KnownNat n => [Round n] -> Views RoundView n -> Views ViewPlaying n
makeViews rs = Views.map $ \i roundView ->
  MakeViewPlaying
    { rounds = map (Round.rotate i) rs,
      current = roundView
    }

hand :: ViewPlaying n -> Cards
hand = Round.View.hand . current

variant :: ViewPlaying n -> Variant
variant = Round.View.variant . current

leader :: ViewPlaying n -> Finite n
leader = Round.View.leader . current

playedCards :: KnownNat n => ViewPlaying n -> Vector n (Maybe Card)
playedCards = Round.View.playedCards . current

tricks :: ViewPlaying n -> [Trick n]
tricks = Round.View.tricks . current
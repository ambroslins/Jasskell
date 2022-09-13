module Jasskell.Round.State
  ( RoundState,
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
import Jasskell.Trick.State qualified as Trick.State
import Jasskell.Variant (Variant)

data RoundState n = RoundState
  { hands :: Vector n Cards,
    tricks :: [Trick n],
    variant :: Variant,
    leader :: Finite n,
    cards :: [Card]
  }
  deriving (Eq, Show)

fromTrick :: [Trick n] -> Trick.State.TrickState n -> RoundState n
fromTrick tricks ts =
  RoundState
    { hands = Trick.State.hands ts,
      tricks = tricks,
      variant = Trick.State.variant ts,
      leader = Trick.State.leader ts,
      cards = Trick.State.cards ts
    }
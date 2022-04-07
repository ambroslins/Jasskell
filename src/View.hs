module View
  ( View,
    Views,
    Phase (..),
    makePlaying,
    makeDeclaring,
    hand,
    leader,
    current,
    variant,
  )
where

import Card
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Variant (Variant)

data Phase
  = Playing
  | Declaring
  deriving (Show)

data View phase n = View
  { hand :: Cards,
    leader :: Finite n,
    phase :: PhaseView phase n
  }
  deriving (Show)

type Views phase n = Finite n -> View phase n

data PhaseView :: Phase -> Nat -> Type where
  PhasePlaying :: Variant -> [Card] -> PhaseView 'Playing n
  PhaseDeclaring :: PhaseView 'Declaring n

deriving instance Show (PhaseView phase n)

current :: KnownNat n => View phase n -> Finite n
current View {leader, phase} = case phase of
  PhasePlaying _ cs -> leader + fromIntegral (length cs)
  PhaseDeclaring -> leader

variant :: View 'Playing n -> Variant
variant View {phase} = case phase of
  PhasePlaying v _ -> v

makePlaying :: KnownNat n => Vector n Cards -> Finite n -> Variant -> [Card] -> Views 'Playing n
makePlaying hands l v cs i =
  View
    { hand = Vector.index hands i,
      leader = l - i,
      phase = PhasePlaying v cs
    }

makeDeclaring :: KnownNat n => Vector n Cards -> Finite n -> Views 'Declaring n
makeDeclaring hands l i =
  View
    { hand = Vector.index hands i,
      leader = l - i,
      phase = PhaseDeclaring
    }

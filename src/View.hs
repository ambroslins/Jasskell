module View
  ( View,
    Views,
    Phase (..),
    makePlaying,
    makeDeclaring,
    hand,
    leader,
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
  PhasePlaying :: Variant -> PhaseView 'Playing n
  PhaseDeclaring :: PhaseView 'Declaring n

deriving instance Show (PhaseView phase n)

variant :: View 'Playing n -> Variant
variant View {phase} = case phase of
  PhasePlaying v -> v

makePlaying :: KnownNat n => Vector n Cards -> Finite n -> Variant -> Views 'Playing n
makePlaying hands l v i =
  View
    { hand = Vector.index hands i,
      leader = l - i,
      phase = PhasePlaying v
    }

makeDeclaring :: KnownNat n => Vector n Cards -> Finite n -> Views 'Declaring n
makeDeclaring hands l i =
  View
    { hand = Vector.index hands i,
      leader = l - i,
      phase = PhaseDeclaring
    }

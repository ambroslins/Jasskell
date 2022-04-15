module View.Declaring
  ( Declaring,
    hand,
    eldest,
    make,
  )
where

import Card (Cards)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector

data Declaring n = Declaring
  { hand :: Cards,
    eldest :: Finite n
  }
  deriving (Show)

make :: KnownNat n => Vector n Cards -> Finite n -> Finite n -> Declaring n
make hands absoluteEldest i =
  Declaring
    { hand = Vector.index hands i,
      eldest = absoluteEldest - i
    }

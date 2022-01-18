module Round.Record
  ( Record (tricks),
    make,
  )
where

import Data.Vector.Sized (Vector)
import GHC.TypeNats (Div)
import Trick (Trick)

newtype Record n = Record
  {tricks :: Vector (Div 36 n) (Trick n)}
  deriving (Show)

make :: Vector (Div 36 n) (Trick n) -> Record n
make = Record
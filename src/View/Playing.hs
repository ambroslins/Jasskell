module View.Playing
  ( Playing,
    hand,
    variant,
    leader,
    table,
    current,
    make,
  )
where

import Card (Card, Cards)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Variant (Variant)

data Playing n = Playing
  { hand :: Cards,
    variant :: Variant,
    leader :: Finite n,
    table :: [Card]
  }
  deriving (Show)

make :: KnownNat n => Vector n Cards -> Finite n -> Variant -> [Card] -> Finite n -> Playing n
make hands absoluteLeader variant table i =
  Playing
    { hand = Vector.index hands i,
      variant,
      leader = absoluteLeader - i,
      table
    }

current :: KnownNat n => Playing n -> Finite n
current Playing {leader, table} = leader + fromIntegral (length table)

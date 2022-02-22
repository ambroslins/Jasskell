module Trick
  ( Trick,
    variant,
    leader,
    cards,
    make,
    winner,
    value,
  )
where

import Card (Card)
import Card qualified
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits (KnownNat)
import JassNat (JassNat)
import Variant (Variant)

data Trick n = Trick
  { variant :: Variant,
    leader :: Finite n,
    cards :: Vector n Card
  }
  deriving (Show)

make :: KnownNat n => Variant -> Finite n -> [Card] -> Maybe (Trick n)
make var l cs = Trick var l . rotate (negate l) <$> Vector.fromList cs

winner :: JassNat n => Trick n -> Finite n
winner Trick {leader, cards, variant} = Vector.maxIndexBy (Card.compare variant lead) cards
  where
    lead = Card.suit $ Vector.index cards leader

value :: Trick n -> Int
value Trick {cards, variant} = sum $ Vector.map (Card.value variant) cards

rotate :: KnownNat n => Finite n -> Vector n a -> Vector n a
rotate n v = Vector.generate (\i -> Vector.index v $ i + n)

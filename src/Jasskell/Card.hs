module Jasskell.Card
  ( Card,
    Suit (..),
    Rank (..),
    suit,
    rank,
    make,
    weli,
    CardSet,
    empty,
    null,
    deck,
    deal,
    insert,
    delete,
    member,
    union,
    intersection,
    difference,
    toList,
    fromList,
    filter,
    compare,
    max,
    points,
  )
where

import Data.Bits (Bits (complement), clearBit, setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Ord (Down (..), comparing)
import Data.Vector4 (Vector4 (..))
import Data.Vector4 qualified as Vector4
import Data.Word (Word32, Word64)
import Jasskell.Card.Suit (Suit (..))
import Jasskell.Variant (Direction (..), Variant (..))
import System.Random (RandomGen)
import System.Random qualified as Random
import Prelude hiding (compare, filter, max, null)

data Rank
  = Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Under
  | Over
  | King
  | Ace
  deriving (Eq, Ord, Bounded, Enum, Show)

newtype Card = Card Word32
  deriving (Eq, Show)

suit :: Card -> Suit
suit (Card w) = toEnum $ fromIntegral (w .&. 3)

rank :: Card -> Rank
rank (Card w) = toEnum $ fromIntegral (w `shiftR` 2)

make :: Suit -> Rank -> Card
make s r =
  Card $ fromIntegral $ fromEnum r `shiftL` 2 .|. fromEnum s

weli :: Card
weli = make Bells Six

newtype CardSet = CardSet Word64
  deriving (Eq, Show)

empty :: CardSet
empty = CardSet 0

null :: CardSet -> Bool
null = (== empty)

deck :: CardSet
deck = CardSet 0x0f_ff_ff_ff_ff -- Set the lower 36 bits

deal :: forall g. (RandomGen g) => g -> (Vector4 CardSet, g)
deal gen =
  let (cards, gen') = coerce $ Random.uniformShuffleList @g @Word32 [0 .. 35] gen
   in ( go (Vector4.replicate empty) cards,
        gen'
      )
  where
    go :: Vector4 CardSet -> [Card] -> Vector4 CardSet
    go !acc = \case
      [] -> acc
      c1 : c2 : c3 : c4 : cs ->
        go (liftA2 insert (Vector4.make c1 c2 c3 c4) acc) cs
      _ -> error "Jasskel.Card.deal: not enough cards"

insert :: Card -> CardSet -> CardSet
insert (Card c) (CardSet cs) = CardSet $ cs `setBit` fromIntegral c

delete :: Card -> CardSet -> CardSet
delete (Card c) (CardSet cs) = CardSet $ cs `clearBit` fromIntegral c

member :: Card -> CardSet -> Bool
member (Card c) (CardSet cs) = cs `testBit` fromIntegral c

union :: CardSet -> CardSet -> CardSet
union (CardSet a) (CardSet b) = CardSet $ a .|. b

intersection :: CardSet -> CardSet -> CardSet
intersection (CardSet a) (CardSet b) = CardSet $ a .&. b

difference :: CardSet -> CardSet -> CardSet
difference (CardSet a) (CardSet b) = CardSet $ a .&. complement b

toList :: CardSet -> [Card]
toList cs = List.filter (`member` cs) $ map Card [0 .. 35]

fromList :: [Card] -> CardSet
fromList = foldl' (flip insert) empty

filter :: (Card -> Bool) -> CardSet -> CardSet
filter f cs = fromList $ List.filter f $ toList cs

compare :: Suit -> Variant -> Card -> Card -> Ordering
compare lead variant = case variant of
  Trump trump ->
    let puur = make trump Under
        nell = make trump Nine
     in comparing (== puur)
          <> comparing (== nell)
          <> comparing (\c -> suit c == trump)
          <> compareDirection BottomUp
  Direction d -> compareDirection d
  Slalom d -> compareDirection d
  where
    compareDirection d =
      comparing (\c -> suit c == lead)
        <> case d of
          BottomUp -> comparing rank
          TopDown -> comparing (Down . rank)

max :: Suit -> Variant -> Card -> Card -> Card
max lead variant c1 c2 = if compare lead variant c1 c2 == LT then c2 else c1

points :: Variant -> Card -> Int
points v c = case v of
  Trump trump
    | c == make trump Under -> 20
    | c == make trump Nine -> 14
  Direction _ | rank c == Eight -> 8
  Slalom _ | rank c == Eight -> 8
  _ -> case rank c of
    Six -> 0
    Seven -> 0
    Eight -> 0
    Nine -> 0
    Ten -> 10
    Under -> 2
    Over -> 3
    King -> 4
    Ace -> 11

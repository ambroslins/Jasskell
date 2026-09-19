module Jasskell.Card.Set
  ( CardSet,
    empty,
    null,
    notNull,
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
    filterSuit,
  )
where

import Data.Bits (clearBit, complement, setBit, testBit, (.&.), (.|.))
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Vector4 (Vector4 (..))
import Data.Vector4 qualified as Vector4
import Jasskell.Card.Internal
import System.Random (RandomGen)
import System.Random qualified as Random
import Prelude hiding (compare, filter, max, null)

null :: CardSet -> Bool
null = (== empty)

notNull :: CardSet -> Bool
notNull = not . null

deal :: forall g. (RandomGen g) => g -> (Vector4 CardSet, g)
deal gen =
  let (cards, gen') = coerce $ Random.uniformShuffleList @g @Int [0 .. 35] gen
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

filterSuit :: Suit -> CardSet -> CardSet
filterSuit s (CardSet cs) = CardSet $ cs .&. suitMask s

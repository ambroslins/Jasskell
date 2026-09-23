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
    foldr,
    foldl',
    toList,
    fromList,
    filter,
    filterSuit,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Vector4 (Vector4 (..))
import Data.Vector4 qualified as Vector4
import Jasskell.Card.Internal
import System.Random (RandomGen)
import System.Random qualified as Random
import Prelude hiding (compare, filter, foldl', foldr, null)

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
insert (Card c) (CardSet cs) = CardSet $ cs `Bits.setBit` fromIntegral c

delete :: Card -> CardSet -> CardSet
delete (Card c) (CardSet cs) = CardSet $ cs `Bits.clearBit` fromIntegral c

member :: Card -> CardSet -> Bool
member (Card c) (CardSet cs) = cs `Bits.testBit` fromIntegral c

union :: CardSet -> CardSet -> CardSet
union (CardSet a) (CardSet b) = CardSet $ a .|. b

intersection :: CardSet -> CardSet -> CardSet
intersection (CardSet a) (CardSet b) = CardSet $ a .&. b

difference :: CardSet -> CardSet -> CardSet
difference (CardSet a) (CardSet b) = CardSet $ a .&. Bits.complement b

foldr :: (Card -> a -> a) -> a -> CardSet -> a
foldr f z (CardSet w) = go w
  where
    go !cs
      | cs == 0 = z
      | otherwise =
          let !c = Card $ Bits.countTrailingZeros cs
           in f c $ go (cs .&. (cs - 1))

foldl' :: (a -> Card -> a) -> a -> CardSet -> a
foldl' f z (CardSet w) = go z w
  where
    go !x !cs
      | cs == 0 = x
      | otherwise =
          let !c = Card $ Bits.countTrailingZeros cs
           in go (f x c) (cs .&. (cs - 1))

toList :: CardSet -> [Card]
toList = foldr (:) []

fromList :: [Card] -> CardSet
fromList = List.foldl' (flip insert) empty

filter :: (Card -> Bool) -> CardSet -> CardSet
filter p = foldl' (\cs c -> if p c then insert c cs else cs) empty

filterSuit :: Suit -> CardSet -> CardSet
filterSuit s (CardSet cs) = CardSet $ cs .&. suitMask s

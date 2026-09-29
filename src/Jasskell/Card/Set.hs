module Jasskell.Card.Set
  ( CardSet,
    empty,
    null,
    notNull,
    size,
    deck,
    shuffle,
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

import Control.Monad (forM_)
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Data.List qualified as List
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Jasskell.Card.Internal
import System.Random (RandomGen)
import System.Random.Stateful qualified as Random
import Prelude hiding (compare, filter, foldl', foldr, null)

null :: CardSet -> Bool
null = (== empty)

notNull :: CardSet -> Bool
notNull = not . null

size :: CardSet -> Int
size (CardSet cs) = Bits.popCount cs

shuffle :: forall g. (RandomGen g) => g -> (VU.Vector Card, g)
shuffle gen = Random.runSTGen gen $ \stGen -> do
  v <- VUM.generate 36 Card
  forM_ [35, 34 .. 1] $ \i -> do
    j <- Random.uniformRM (0, i) stGen
    VUM.swap v i j
  VU.unsafeFreeze v

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

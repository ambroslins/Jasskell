{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Jasskell.Card.Internal
  ( Suit (..),
    Rank (..),
    Card (..),
    suit,
    rank,
    make,
    CardSet (..),
    empty,
    deck,
    ofSuit,
    ofRank,
    foldr,
    foldl',
    Sequence (..),
    sequences,
    quartets,
  )
where

import Control.Exception (assert)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Data.Int (Int64)
import Data.Vec qualified as Vec
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive (Prim)
import Data.Vector.Unboxed qualified as VU
import GHC.List qualified as List
import Prelude hiding (compare, filter, foldl', foldr, max, null)

data Suit = Bells | Hearts | Acorns | Leaves
  deriving (Eq, Ord, Enum, Bounded, Show)

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

newtype Card = Card Int
  deriving newtype (Eq, Show, Prim, Vec.Elem)

newtype instance VU.MVector s Card = MV_Card (VU.MVector s Int)

newtype instance VU.Vector Card = V_Card (VU.Vector Int)

deriving instance VGM.MVector VU.MVector Card

deriving instance VG.Vector VU.Vector Card

instance VU.Unbox Card

suit :: Card -> Suit
suit (Card c) = toEnum (c .&. 3)

rank :: Card -> Rank
rank (Card c) = toEnum (c `Bits.unsafeShiftR` 2)

make :: Suit -> Rank -> Card
make s r =
  Card $ fromEnum r `Bits.unsafeShiftL` 2 .|. fromEnum s

newtype CardSet = CardSet Int64
  deriving newtype (Eq, Show, Vec.Elem)

empty :: CardSet
empty = CardSet 0

deck :: CardSet
deck = CardSet 0x0f_ff_ff_ff_ff -- Set the lower 36 bits

ofSuit :: Suit -> CardSet
ofSuit s = CardSet (0x01_11_11_11_11 `Bits.unsafeShiftL` fromEnum s)

ofRank :: Rank -> CardSet
ofRank r = CardSet (0x0f `Bits.unsafeShiftL` (4 * fromEnum r))

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

data Sequence = Sequence {high :: Card, length :: Int}
  deriving (Eq, Show)

sequences :: CardSet -> [Sequence]
sequences (CardSet cs) = List.build $
  \cons nil -> foldr (cons . toSequence) nil (CardSet ends)
  where
    toSequence c =
      let len = sequenceLength 0 c
       in assert (len >= 3) $ Sequence {high = c, length = len}
    ends =
      cs
        .&. cs `Bits.unsafeShiftL` 4
        .&. cs `Bits.unsafeShiftL` 8
        .&. Bits.complement (cs `Bits.unsafeShiftR` 4)
    sequenceLength i (Card c)
      | c >= 0 && cs `Bits.testBit` c = sequenceLength (i + 1) (Card $ c - 4)
      | otherwise = i

quartets :: CardSet -> [Rank]
quartets (CardSet cs) = List.build $
  \cons nil -> foldr (cons . rank) nil (CardSet ranks)
  where
    m1 = cs .&. cs `Bits.unsafeShiftR` 1
    m2 = m1 .&. m1 `Bits.unsafeShiftR` 2
    ranks = m2 .&. 0x01_11_11_10_00

$(deriveJSON defaultOptions ''Rank)
$(deriveJSON defaultOptions ''Suit)
$(deriveJSON defaultOptions ''Card)

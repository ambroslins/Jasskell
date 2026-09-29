{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
    suitMask,
  )
where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive (Prim)
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import Prelude hiding (compare, filter, max, null)

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
  deriving (Eq, Show, Prim)

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

newtype CardSet = CardSet Word64
  deriving (Eq, Show)

empty :: CardSet
empty = CardSet 0

deck :: CardSet
deck = CardSet 0x0f_ff_ff_ff_ff -- Set the lower 36 bits

suitMask :: Suit -> Word64
suitMask s = 0x01_11_11_11_11 `Bits.shiftL` fromEnum s

$(deriveJSON defaultOptions ''Rank)
$(deriveJSON defaultOptions ''Suit)
$(deriveJSON defaultOptions ''Card)

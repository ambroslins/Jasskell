module Data.BitSet
  ( BitSet,
    empty,
    index,
    set,
    clear,
    insert,
    toWord64,
    fromWord64,
  )
where

import Data.Bits qualified as Bits
import Data.Fin (Fin)
import Data.Fin qualified as Fin
import Data.Word (Word64)
import GHC.TypeLits (Nat, type (<=))
import GHC.TypeNats (KnownNat)

newtype BitSet (n :: Nat) = BitSet Word64
  deriving (Eq, Show)

type Size n = (KnownNat n, n <= 64)

empty :: forall n. (Size n) => BitSet n
empty = BitSet 0

index :: forall n. BitSet n -> Fin n -> Bool
index (BitSet s) i = s `Bits.testBit` Fin.toInt i

set :: forall n. Fin n -> BitSet n -> BitSet n
set i (BitSet s) = BitSet $ s `Bits.setBit` Fin.toInt i

clear :: forall n. Fin n -> BitSet n -> BitSet n
clear i (BitSet s) = BitSet $ s `Bits.clearBit` Fin.toInt i

insert :: forall n. Fin n -> Bool -> BitSet n -> BitSet n
insert i b = if b then set i else clear i

toWord64 :: BitSet n -> Word64
toWord64 (BitSet s) = s

-- | 'Nothing' if a bit at index @n@ or higher is set.
fromWord64 :: forall n. (Size n) => Word64 -> Maybe (BitSet n)
fromWord64 s
  | s `Bits.shiftR` Fin.nat n == 0 = Just $ BitSet s
  | otherwise = Nothing

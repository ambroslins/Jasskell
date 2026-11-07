{- HLINT ignore "Use foldM_" -}
module Jasskell.Card.Seq
  ( CardSeq,
    empty,
    null,
    length,
    push,
    index,
    unsafeIndex,
    foldr,
    foldl',
    foldM,
    foldM_,
    toList,
    fromList,
    maxIndexBy,
  )
where

import Control.Exception (assert)
import Control.Monad (void)
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Data.Coerce (coerce)
import Data.Word (Word64)
import GHC.List qualified as List
import Jasskell.Card.Internal (Card (..))
import Prelude hiding (foldl', foldr, length, null)

-- | Bit packed sequence of cards
newtype CardSeq = CardSeq Word64
  deriving (Eq)

instance Show CardSeq where
  show = show . toList

bitsPerCard :: Int
bitsPerCard = 6

mask :: Word64
mask = 0x3f

empty :: CardSeq
empty = CardSeq 0

null :: CardSeq -> Bool
null = (== empty)

length :: CardSeq -> Int
length (CardSeq s) = fromIntegral $ s `Bits.unsafeShiftR` 60

push :: Card -> CardSeq -> CardSeq
push (Card c) cs@(CardSeq s) =
  assert (len < 10) $ CardSeq $ (s .|. fromIntegral c `Bits.unsafeShiftL` (len * bitsPerCard)) + length1
  where
    !len = length cs
    !length1 = 1 `Bits.unsafeShiftL` 60

unsafeIndex :: CardSeq -> Int -> Card
unsafeIndex (CardSeq s) i =
  Card $ fromIntegral $ (s `Bits.shiftR` (i * bitsPerCard)) .&. mask

index :: CardSeq -> Int -> Maybe Card
index cs i
  | i >= 0 && i < length cs = Just $! unsafeIndex cs i
  | otherwise = Nothing

foldr :: (Card -> a -> a) -> a -> CardSeq -> a
foldr f z cs = go (length cs) (coerce cs)
  where
    go !l !s
      | l > 0 =
          let c = Card $ fromIntegral $ s .&. mask
           in f c $ go (l - 1) (s `Bits.unsafeShiftR` bitsPerCard)
      | otherwise = z

foldl' :: (a -> Card -> a) -> a -> CardSeq -> a
foldl' f z cs = go (length cs) z (coerce cs)
  where
    go !l !x !s
      | l > 0 =
          let !c = Card $ fromIntegral $ s .&. mask
           in go (l - 1) (f x c) (s `Bits.unsafeShiftR` bitsPerCard)
      | otherwise = x

foldM :: (Monad m) => (b -> Card -> m b) -> b -> CardSeq -> m b
foldM f z0 cs = foldr go pure cs z0
  where
    go c k x = f x c >>= k

foldM_ :: (Monad m) => (b -> Card -> m b) -> b -> CardSeq -> m ()
foldM_ f z = void . foldM f z

toList :: CardSeq -> [Card]
toList cs = List.build $ \cons nil -> foldr cons nil cs

fromList :: [Card] -> CardSeq
fromList = List.foldl' (flip push) empty

maxIndexBy :: (Card -> Card -> Ordering) -> CardSeq -> Int
maxIndexBy cmp cs
  | null cs = error "Jasskell.Card.Seq: maxIndexBy on empty"
  | otherwise = go 1 0 (unsafeIndex cs 0)
  where
    len = length cs
    go !i !mi !mc
      | i < len =
          let c = unsafeIndex cs i
           in case cmp mc c of
                LT -> go (i + 1) i c
                _ -> go (i + 1) mi mc
      | otherwise = mi

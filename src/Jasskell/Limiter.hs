{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Jasskell.Limiter
  ( Limiter,
    Config (..),
    newLimiter,
    allowN,
  )
where

import Control.Monad.Primitive (RealWorld)
import Data.Bits ((.&.))
import Data.Bits qualified as Bits
import Data.Hashable (Hashable (hashWithSalt))
import Data.Primitive.ByteArray
  ( MutableByteArray (..),
    newAlignedPinnedByteArray,
    readByteArray,
    setByteArray,
  )
import GHC.Exts (Int (..), casInt64Array#)
import GHC.IO (IO (..))
import GHC.Int (Int64 (..))
import Jasskell.Clock (Duration, MonotonicClock, MonotonicTime (..), nanoseconds)
import Jasskell.Clock qualified as Clock
import System.Random.Stateful qualified as Random

data Config = Config
  { slots :: Int,
    rate :: Double,
    burst :: Int
  }
  deriving (Show)

data Limiter a = Limiter
  { slots :: MutableByteArray RealWorld,
    mask :: Int,
    salt :: Int,
    interval :: Duration,
    burst :: Duration,
    clock :: MonotonicClock
  }

newLimiter :: MonotonicClock -> Config -> IO (Limiter a)
newLimiter clock config = do
  salt <- Random.uniformM Random.globalStdGen
  slots <- newAlignedPinnedByteArray (size * 8) 64
  now <- Clock.getMonotonicTime clock
  setByteArray slots 0 size now
  pure $
    Limiter
      { slots,
        mask = size - 1,
        salt,
        interval,
        burst = Clock.scale interval config.burst,
        clock
      }
  where
    -- Round size to the next power of two
    size = 1 `Bits.shiftL` (Bits.finiteBitSize config.slots - Bits.countLeadingZeros (config.slots - 1))
    interval = nanoseconds $ round (1e9 / config.rate) `max` 1

allowN :: (Hashable a) => Limiter a -> a -> Int -> IO Bool
allowN limiter key n = do
  now <- Clock.getMonotonicTime limiter.clock
  let go !tat =
        let newTat = Clock.addMonotonic (max tat now) (Clock.scale limiter.interval n)
         in if Clock.elapsed now newTat > limiter.burst
              then pure False
              else do
                prev <- casTimestamp limiter.slots i tat newTat
                if prev == tat then pure True else go prev
  readByteArray limiter.slots i >>= go
  where
    h = hashWithSalt limiter.salt key
    i = h .&. limiter.mask

casTimestamp :: MutableByteArray RealWorld -> Int -> MonotonicTime -> MonotonicTime -> IO MonotonicTime
casTimestamp (MutableByteArray mba) (I# i) (MonotonicTime (I64# old)) (MonotonicTime (I64# new)) =
  IO
    ( \s -> case casInt64Array# mba i old new s of
        (# s', prev #) -> (# s', MonotonicTime (I64# prev) #)
    )
{-# INLINE casTimestamp #-}

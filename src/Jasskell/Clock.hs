{-# LANGUAGE FieldSelectors #-}

module Jasskell.Clock
  ( Clock,
    now,
    delayed,
    monotonic,
    Timestamp (..),
    Duration (..),
    add,
    diff,
    scale,
    timed,
    nanoseconds,
    microseconds,
    milliseconds,
    seconds,
  )
where

import Control.Concurrent.STM (STM, registerDelay, retry)
import Control.Concurrent.STM.TVar (readTVar)
import Data.Aeson (ToJSON (..))
import Data.Int (Int64)
import Data.Primitive (Prim)
import GHC.Clock (getMonotonicTimeNSec)

newtype Timestamp = Timestamp Int64
  deriving newtype (Eq, Ord, Show, Prim)

data Clock = Clock
  { now :: IO Timestamp,
    delayed :: forall a. Duration -> a -> IO (STM a)
  }

monotonic :: Clock
monotonic =
  Clock
    { now = Timestamp . fromIntegral <$> getMonotonicTimeNSec,
      delayed = \(Duration d) x -> do
        delay <- registerDelay (fromIntegral d `div` 1000)
        pure $ do
          done <- readTVar delay
          if done then pure x else retry
    }

newtype Duration = Duration Int64
  deriving newtype (Eq, Ord, Show)

instance ToJSON Duration where
  toJSON = \(Duration d) -> toJSON $ if d < 0 then '-' : go (negate d) else go d
    where
      go i
        | i < 995 = show i <> "ns"
        | i < 995_0 = showFixed 3 2 i "us"
        | i < 995_00 = showFixed 3 1 i "us"
        | i < 995_000 = showFixed 3 0 i "us"
        | i < 995_000_0 = showFixed 6 2 i "ms"
        | i < 995_000_00 = showFixed 6 1 i "ms"
        | i < 995_000_000 = showFixed 6 0 i "ms"
        | i < 995_000_000_0 = showFixed 9 2 i "s"
        | i < 995_000_000_00 = showFixed 9 1 i "s"
        | otherwise = showFixed 9 0 i "s"
      showFixed d f i unit
        | f == 0 = show (scaled d i) <> unit
        | otherwise = reverse (frac <> ('.' : dec)) <> unit
        where
          (frac, dec) = splitAt f $ reverse $ show $ scaled (d - f) i

      scaled :: Int -> Int64 -> Int64
      scaled d i = if (rest + rest) >= base then n + 1 else n
        where
          base = 10 ^ d
          (n, rest) = i `quotRem` base

add :: Timestamp -> Duration -> Timestamp
add (Timestamp ts) (Duration d) = Timestamp (ts + d)

diff :: Timestamp -> Timestamp -> Duration
diff (Timestamp a) (Timestamp b) = Duration (a - b)

scale :: Duration -> Int -> Duration
scale (Duration d) n = Duration (d * fromIntegral n)

timed :: Clock -> IO a -> IO (Duration, a)
timed Clock {now} action = do
  start <- now
  !result <- action
  end <- now
  let !d = end `diff` start
  pure (d, result)

nanoseconds, microseconds, milliseconds, seconds :: Int64 -> Duration
nanoseconds = Duration
microseconds = Duration . (* 1_000)
milliseconds = Duration . (* 1_000_000)
seconds = Duration . (* 1_000_000_000)

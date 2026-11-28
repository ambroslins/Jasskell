{-# LANGUAGE FieldSelectors #-}

module Jasskell.Clock
  ( MonotonicClock,
    MonotonicTime (..),
    monotonic,
    getMonotonicTime,
    delayed,
    timed,
    sleep,
    SystemClock,
    SystemTime (..),
    system,
    getSystemTime,
    addSystem,
    UTCTime (..),
    Data.Time.Clock.System.systemToUTCTime,
    getUTCTime,
    Duration (..),
    addMonotonic,
    elapsed,
    scale,
    nanoseconds,
    microseconds,
    milliseconds,
    seconds,
    minutes,
    hours,
    days,
  )
where

import Control.Concurrent.STM (STM, registerDelay, retry)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TVar (readTVar)
import Data.Aeson (ToJSON (..))
import Data.Int (Int64)
import Data.Primitive (Prim)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.System (SystemTime (..))
import Data.Time.Clock.System qualified
import GHC.Clock (getMonotonicTimeNSec)

newtype MonotonicTime = MonotonicTime Int64
  deriving newtype (Eq, Ord, Show, Prim)

data MonotonicClock = MonotonicClock
  { getMonotonicTime :: IO MonotonicTime,
    delayed :: forall a. Duration -> a -> IO (STM a)
  }

monotonic :: MonotonicClock
monotonic =
  MonotonicClock
    { getMonotonicTime = MonotonicTime . fromIntegral <$> getMonotonicTimeNSec,
      delayed = \(Duration d) x -> do
        delay <- registerDelay (fromIntegral d `div` 1000)
        pure $ do
          done <- readTVar delay
          if done then pure x else retry
    }

timed :: MonotonicClock -> IO a -> IO (Duration, a)
timed MonotonicClock {getMonotonicTime} action = do
  start <- getMonotonicTime
  !result <- action
  end <- getMonotonicTime
  let !d = elapsed start end
  pure (d, result)

sleep :: MonotonicClock -> Duration -> IO ()
sleep clock d = do
  delay <- delayed clock d ()
  STM.atomically delay

addMonotonic :: MonotonicTime -> Duration -> MonotonicTime
addMonotonic (MonotonicTime ts) (Duration d) = MonotonicTime (ts + d)

elapsed :: MonotonicTime -> MonotonicTime -> Duration
elapsed (MonotonicTime start) (MonotonicTime end) = Duration (end - start)

newtype SystemClock = SystemClock {getSystemTime :: IO SystemTime}

getUTCTime :: SystemClock -> IO UTCTime
getUTCTime = fmap Data.Time.Clock.System.systemToUTCTime . getSystemTime

system :: SystemClock
system = SystemClock Data.Time.Clock.System.getSystemTime

addSystem :: SystemTime -> Duration -> SystemTime
addSystem t (Duration d) =
  MkSystemTime
    { systemSeconds = systemSeconds t + s + if carry then 1 else 0,
      systemNanoseconds = if carry then systemNs else systemNs - nanosecondsInSecond
    }
  where
    (s, ns) = d `divMod` nanosecondsInSecond
    systemNs = systemNanoseconds t + fromIntegral ns
    carry = systemNs >= nanosecondsInSecond

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

scale :: Duration -> Int -> Duration
scale (Duration d) n = Duration (d * fromIntegral n)

nanosecondsInSecond :: (Num a) => a
nanosecondsInSecond = 1_000_000_000

nanoseconds, microseconds, milliseconds, seconds, minutes, hours, days :: Int64 -> Duration
nanoseconds = Duration
microseconds = nanoseconds . (* 1_000)
milliseconds = nanoseconds . (* 1_000_000)
seconds = nanoseconds . (* nanosecondsInSecond)
minutes = seconds . (* 60)
hours = minutes . (* 60)
days = hours . (* 24)

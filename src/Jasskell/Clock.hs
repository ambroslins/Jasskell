{-# LANGUAGE FieldSelectors #-}

module Jasskell.Clock
  ( Clock,
    now,
    delayed,
    systemClock,
    Timestamp,
    Duration,
    diff,
    nanoseconds,
    microseconds,
    milliseconds,
    seconds,
  )
where

import Control.Concurrent.STM (STM, registerDelay, retry)
import Control.Concurrent.STM.TVar (readTVar)
import Data.Int (Int64)
import Data.Primitive (Prim)
import GHC.Clock (getMonotonicTimeNSec)

newtype Timestamp = Timestamp Int64
  deriving newtype (Eq, Ord, Show, Prim)

data Clock = Clock
  { now :: IO Timestamp,
    delayed :: forall a. Duration -> a -> IO (STM a)
  }

systemClock :: Clock
systemClock =
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

diff :: Timestamp -> Timestamp -> Duration
diff (Timestamp a) (Timestamp b) = Duration (a - b)

nanoseconds, microseconds, milliseconds, seconds :: Int64 -> Duration
nanoseconds = Duration
microseconds = Duration . (* 1_000)
milliseconds = Duration . (* 1_000_000)
seconds = Duration . (* 1_000_000_000)

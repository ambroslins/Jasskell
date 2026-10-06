{-# LANGUAGE FieldSelectors #-}

-- | A poor man's version of [Bluefin.Capability.Throw](https://hackage-content.haskell.org/package/bluefin-0.10.1.0/docs/Bluefin-Capability-Throw.html).
-- Note that nested throws are correctly handle since we create a unique tag
-- for each scope.
-- Unlike bluefin (and other effect systems), we don't prevent the effect to escape.
module Jasskell.Throw
  ( Throw,
    throw,
    trying,
    handling,
    catching,
  )
where

import Control.Exception (Exception, catch, throwIO)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Unique (Unique, newUnique)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

newtype Throw e = Throw {throw :: forall a. e -> IO a}

instance Contravariant Throw where
  contramap f (Throw t) = Throw (t . f)

data Escape = Escape Unique Any

instance Show Escape where
  show _ = "Throw.Escape (escaped its handler)"

instance Exception Escape

trying :: (Throw e -> IO a) -> IO (Either e a)
trying body = do
  tag <- newUnique
  let throwing = Throw $ \e -> throwIO (Escape tag $ unsafeCoerce e)
  (Right <$> body throwing) `catch` \ex@(Escape t e) ->
    if t == tag
      then pure . Left $ unsafeCoerce e
      else throwIO ex

handling :: (e -> IO a) -> (Throw e -> IO a) -> IO a
handling h body = trying body >>= either h pure

catching :: (Throw e -> IO a) -> (e -> IO a) -> IO a
catching = flip handling

{-# LANGUAGE FieldSelectors #-}

module Jasskell.Throw
  ( Throw,
    throw,
    trying,
    handling,
    catching,
    earlyReturn,
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

earlyReturn :: (Throw a -> IO a) -> IO a
earlyReturn = handling pure

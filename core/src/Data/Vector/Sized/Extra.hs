module Data.Vector.Sized.Extra
  ( rotate,
    unfoldrM,
    iterateM,
    constructM,
  )
where

import Control.Monad.ST (runST)
import Data.Finite (Finite)
import Data.Finite qualified as Finite
import Data.Type.Equality (type (:~:) (Refl))
import Data.Vector.Mutable.Sized qualified as Vector.Mutable
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeNats (type (+))
import Relude.Extra (fmapToSnd)
import Unsafe.Coerce (unsafeCoerce)

rotate :: KnownNat n => Finite n -> Vector n a -> Vector n a
rotate n v = Vector.generate (\i -> Vector.index v $ i + n)

unfoldrM :: (KnownNat n, Monad m) => (b -> m (a, b)) -> b -> m (Vector n a)
unfoldrM f =
  evalStateT $
    Vector.replicateM $ do
      x <- get
      (y, z) <- lift (f x)
      put z
      pure y

iterateM ::
  forall n a m.
  (KnownNat n, Monad m) =>
  (a -> m a) ->
  a ->
  m (Vector n a)
iterateM f = unfoldrM (fmapToSnd f) . pure

constructM ::
  forall n a m.
  (KnownNat n, Monad m) =>
  (forall i. KnownNat i => Vector i a -> m a) ->
  m (Vector n a)
constructM f = foldlM go nil Finite.finites
  where
    nil = runST $ Vector.Mutable.new @n >>= Vector.unsafeFreeze
    go :: Vector n a -> Finite n -> m (Vector n a)
    go v i = case someNatVal $ fromIntegral i of
      SomeNat (Proxy :: Proxy i) ->
        case unsafeCoerce Refl :: (i + j + 1) :~: n of
          Refl -> do
            x <- f $ Vector.take @i v
            pure $
              runST $ do
                mut <- Vector.unsafeThaw v
                Vector.Mutable.write mut i x
                Vector.unsafeFreeze mut

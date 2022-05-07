module Data.Vector.Sized.Extra
  ( rotate,
    unfoldrM,
    iterateM,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Relude.Extra (fmapToSnd)

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

iterateM :: forall n a m. (KnownNat n, Monad m) => (a -> m a) -> a -> m (Vector n a)
iterateM f = unfoldrM (fmapToSnd f) . pure
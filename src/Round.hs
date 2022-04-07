module Round
  ( Round,
    tricks,
    play,
  )
where

import Card (Cards)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeNats (Div)
import Jass (MonadJass, promptVariant)
import Trick (Trick)
import Trick qualified
import Variant qualified

newtype Round n = Round {tricks :: Vector (Div 36 n) (Trick n)}
  deriving (Show)

play :: (MonadJass n m) => Finite n -> Vector n Cards -> m (Round n)
play leader = evalStateT $ do
  variant <- promptVariant undefined
  firstTrick <- Trick.play variant leader
  let playTrick t = Trick.play (Variant.next $ Trick.variant t) (Trick.winner t)
  Round <$> iterateM playTrick firstTrick

iterateM :: (KnownNat n, Monad m) => (a -> m a) -> a -> m (Vector n a)
iterateM f = evalStateT $
  Vector.replicateM $ do
    x <- get
    y <- lift (f x)
    put y
    pure y
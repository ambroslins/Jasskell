module Trick
  ( Trick,
    play,
    variant,
    leader,
    cards,
    winner,
    points,
  )
where

import Card (Card, Cards)
import Card qualified
import Data.Finite (Finite)
import Data.Set qualified as Set
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jass (JassNat, MonadJass, promptCard)
import Relude.Extra.Lens (over)
import Variant (Variant)

data Trick n = Trick
  { variant :: Variant,
    leader :: Finite n,
    cards :: Vector n Card
  }
  deriving (Eq, Show)

play :: (MonadJass n m, MonadState (Vector n Cards) m) => Variant -> Finite n -> m (Trick n)
play variant leader = close <$> unfoldrM playCard []
  where
    close cs = Trick {variant, leader, cards = rotate (negate leader) cs}
    playCard cs = do
      let current = leader + fromIntegral (length cs)
          view = undefined
      card <- promptCard view
      modify $ over (Vector.ix current) (Set.delete card)
      pure (card, cs ++ [card])

winner :: JassNat n => Trick n -> Finite n
winner Trick {leader, cards, variant} = Vector.maxIndexBy (Card.compare variant lead) cards
  where
    lead = Card.suit $ Vector.index cards leader

points :: Trick n -> Int
points Trick {cards, variant} = sum $ Vector.map (Card.value variant) cards

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

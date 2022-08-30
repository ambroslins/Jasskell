module Gen where

import Data.Finite (Finite)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Jasskell.Card (Card (Card), Rank, Suit)
import Jasskell.Variant (Direction (..), Variant (..))

orderedPair :: (Ord a, MonadGen m) => m a -> m (a, a)
orderedPair g = do
  x <- g
  y <- g
  pure (min x y, max x y)

suit :: MonadGen m => m Suit
suit = Gen.enumBounded

rank :: MonadGen m => m Rank
rank = Gen.enumBounded

card :: MonadGen m => m Card
card = Card <$> suit <*> rank

direction :: MonadGen m => m Direction
direction = Gen.element [TopDown, BottomUp]

variant :: MonadGen m => m Variant
variant =
  Gen.choice
    [ Trump <$> suit,
      Direction <$> direction,
      Slalom <$> direction
    ]

finite :: forall n m. (KnownNat n, MonadGen m) => m (Finite n)
finite = Gen.enumBounded

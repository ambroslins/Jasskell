module Jasskell.Trick
  ( Trick,
    play,
    variant,
    leader,
    cards,
    winner,
    points,
  )
where

import Data.Finite (Finite)
import Data.Set qualified as Set
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import GHC.TypeNats (type (+))
import Jasskell.Card (Card, Cards)
import Jasskell.Card qualified as Card
import Jasskell.Variant (Variant)
import Relude.Extra.Lens (over)

data Trick n = Trick
  { variant :: Variant,
    leader :: Finite n,
    cards :: Vector n Card
  }
  deriving (Eq, Show)

play ::
  forall n m.
  (KnownNat n, MonadState (Vector n Cards) m) =>
  ([Card] -> Finite n -> m Card) ->
  Variant ->
  Finite n ->
  m (Trick n)
play promptCard variant leader = close <$> Vector.constructM playCard
  where
    close cs = Trick {variant, leader, cards = Vector.rotate (negate leader) cs}
    playCard :: forall i. KnownNat i => Vector i Card -> m Card
    playCard cards = do
      let current = leader + fromIntegral (Vector.length cards)
      card <- promptCard (Vector.toList cards) current
      modify $ over (Vector.ix current) (Set.delete card)
      pure card

winner :: n ~ (m + 1) => Trick n -> Finite n
winner Trick {leader, cards, variant} =
  Vector.maxIndexBy (Card.compare variant lead) cards
  where
    lead = Card.suit $ Vector.index cards leader

points :: Trick n -> Int
points Trick {cards, variant} = sum $ Vector.map (Card.value variant) cards

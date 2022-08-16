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
import Jasskell.Card (Card, Cards)
import Jasskell.Card qualified as Card
import Jasskell.Jass (JassNat, MonadJass, promptCard)
import Jasskell.Variant (Variant)
import Jasskell.View.Absolute qualified as View.Absolute
import Relude.Extra.Lens (over)

data Trick n = Trick
  { variant :: Variant,
    leader :: Finite n,
    cards :: Vector n Card
  }
  deriving (Eq, Show)

play ::
  forall n m.
  (MonadJass n m, MonadState (Vector n Cards) m) =>
  Variant ->
  Finite n ->
  m (Trick n)
play variant leader = close <$> Vector.constructM playCard
  where
    close cs = Trick {variant, leader, cards = Vector.rotate (negate leader) cs}
    playCard :: forall i. KnownNat i => Vector i Card -> m Card
    playCard table = do
      hands <- get
      let current = leader + fromIntegral (Vector.length table)
          cards =
            Vector.rotate (negate leader) $
              Vector.unfoldrN
                (\case [] -> (Nothing, []); c : cs -> (Just c, cs))
                (Vector.toList table)
          view =
            View.Absolute.MakeView
              { View.Absolute.hands = hands,
                View.Absolute.cards = cards,
                View.Absolute.variant = Just variant,
                View.Absolute.leader = leader
              }
      card <- promptCard current view
      modify $ over (Vector.ix current) (Set.delete card)
      pure card

winner :: JassNat n => Trick n -> Finite n
winner Trick {leader, cards, variant} =
  Vector.maxIndexBy (Card.compare variant lead) cards
  where
    lead = Card.suit $ Vector.index cards leader

points :: Trick n -> Int
points Trick {cards, variant} = sum $ Vector.map (Card.value variant) cards

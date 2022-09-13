module Jasskell.Trick
  ( Trick,
    Interface (..),
    play,
    variant,
    leader,
    cards,
    winner,
    points,
    rotate,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import GHC.TypeNats (type (+))
import Jasskell.Card (BadCard, Card, Cards)
import Jasskell.Card qualified as Card
import Jasskell.Trick.State (TrickState)
import Jasskell.Trick.State qualified as Trick.State
import Jasskell.Variant (Variant)
import Relude.Extra.Lens qualified as Lens

data Trick n = Trick
  { variant :: Variant,
    leader :: Finite n,
    cards :: Vector n Card
  }
  deriving (Eq, Show)

data Interface n m = Interface
  { promptCard :: Finite n -> TrickState n -> m Card,
    throwBadCard :: forall a. BadCard -> m a
  }

play ::
  forall n m.
  (KnownNat n, MonadState (Vector n Cards) m) =>
  Interface n m ->
  Variant ->
  Finite n ->
  m (Trick n)
play Interface {..} variant leader = close <$> Vector.constructM playCard
  where
    close cs = Trick {variant, leader, cards = Vector.rotate (negate leader) cs}

    playCard :: forall i. KnownNat i => Vector i Card -> m Card
    playCard cards = do
      hands <- get
      let currentHand = Vector.index hands current
          trickState =
            Trick.State.TrickState
              { Trick.State.hands = hands,
                Trick.State.variant = variant,
                Trick.State.leader = leader,
                Trick.State.cards = cardList
              }
      card <- promptCard current trickState
      case Card.playable variant cardList currentHand card of
        Left reason -> throwBadCard reason
        Right newHand -> do
          modify $ Lens.set (Vector.ix current) newHand
          pure card
      where
        current = leader + fromIntegral (Vector.length cards)
        cardList = Vector.toList cards

winner :: n ~ (m + 1) => Trick n -> Finite n
winner Trick {leader, cards, variant} =
  Vector.maxIndexBy (Card.compare variant lead) cards
  where
    lead = Card.suit $ Vector.index cards leader

points :: Trick n -> Int
points Trick {cards, variant} = sum $ Vector.map (Card.value variant) cards

rotate :: KnownNat n => Finite n -> Trick n -> Trick n
rotate i trick =
  trick
    { leader = leader trick - i,
      cards = Vector.rotate i $ cards trick
    }

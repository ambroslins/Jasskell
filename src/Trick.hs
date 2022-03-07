module Trick
  ( Playing,
    Closed,
    current,
    new,
    next,
    playCard,
    winner,
    points,
  )
where

import Card (Card, Cards)
import Card qualified
import Control.Monad.Except
import Data.Finite (Finite, modulo)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits (KnownNat)
import JassNat (JassNat)
import List qualified
import Variant (Variant)
import Variant qualified

data TrickState f n = TrickState
  { variant :: Variant,
    leader :: Finite n,
    cards :: f Card
  }

deriving instance Eq (f Card) => Eq (TrickState f n)

deriving instance Show (f Card) => Show (TrickState f n)

newtype Playing n = Playing (TrickState [] n)
  deriving newtype (Eq, Show)

newtype Closed n = Closed (TrickState (Vector n) n)
  deriving newtype (Eq, Show)

current :: KnownNat n => Playing n -> Finite n
current (Playing trick) = trick.leader + modulo (fromIntegral $ length trick.cards)

new :: Variant -> Finite n -> Playing n
new v l =
  Playing $
    TrickState
      { variant = v,
        leader = l,
        cards = []
      }

next :: JassNat n => Closed n -> Playing n
next (Closed trick) =
  Playing $
    TrickState
      { variant = Variant.next trick.variant,
        leader = winner (Closed trick),
        cards = []
      }

playCard :: (MonadError Card.Reason m, KnownNat n) => Cards -> Card -> Playing n -> m (Either (Playing n) (Closed n))
playCard hand card (Playing trick) =
  case Card.status trick.variant trick.cards hand card of
    Card.Unplayable reason -> throwError reason
    Card.Playable -> pure $ case Vector.fromList cs of
      Nothing -> Left $ Playing trick {cards = cs}
      Just vec ->
        Right $
          Closed trick {cards = rotate (negate trick.leader) vec}
  where
    cs = List.snoc trick.cards card

winner :: JassNat n => Closed n -> Finite n
winner (Closed trick) = Vector.maxIndexBy (Card.compare trick.variant lead) trick.cards
  where
    lead = Card.suit $ Vector.index trick.cards trick.leader

points :: Closed n -> Int
points (Closed trick) = Vector.sum $ Vector.map (Card.value trick.variant) trick.cards

rotate :: KnownNat n => Finite n -> Vector n a -> Vector n a
rotate n v = Vector.generate (\i -> Vector.index v $ i + n)

module Trick
  ( Trick (..),
    TrickPlaying,
    TrickClosed,
    variant,
    leader,
    current,
    cards,
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

data Trick n
  = Playing (TrickPlaying n)
  | Closed (TrickClosed n)
  deriving (Eq, Show)

data TrickState f n = TrickState
  { variant :: Variant,
    leader :: Finite n,
    cards :: f Card
  }

deriving instance Eq (f Card) => Eq (TrickState f n)

deriving instance Show (f Card) => Show (TrickState f n)

newtype TrickPlaying n = TrickPlaying (TrickState [] n)
  deriving newtype (Eq, Show)

newtype TrickClosed n = TrickClosed (TrickState (Vector n) n)
  deriving newtype (Eq, Show)

variant :: Trick n -> Variant
variant = \case
  Playing (TrickPlaying trick) -> trick.variant
  Closed (TrickClosed trick) -> trick.variant

leader :: Trick n -> Finite n
leader = \case
  Playing (TrickPlaying trick) -> trick.leader
  Closed (TrickClosed trick) -> trick.leader

current :: KnownNat n => TrickPlaying n -> Finite n
current (TrickPlaying trick) = trick.leader + modulo (fromIntegral $ length trick.cards)

cards :: KnownNat n => Trick n -> Vector n (Maybe Card)
cards = \case
  Playing (TrickPlaying trick) ->
    Vector.accum
      (const Just)
      (Vector.replicate Nothing)
      (List.zip [trick.leader ..] trick.cards)
  Closed (TrickClosed trick) ->
    Just <$> trick.cards

new :: Variant -> Finite n -> TrickPlaying n
new v l =
  TrickPlaying $
    TrickState
      { variant = v,
        leader = l,
        cards = []
      }

next :: JassNat n => TrickClosed n -> TrickPlaying n
next (TrickClosed trick) =
  TrickPlaying $
    TrickState
      { variant = Variant.next trick.variant,
        leader = winner (TrickClosed trick),
        cards = []
      }

playCard :: (MonadError Card.Reason m, KnownNat n) => Cards -> Card -> TrickPlaying n -> m (Trick n)
playCard hand card (TrickPlaying trick) =
  case Card.status trick.variant trick.cards hand card of
    Card.Unplayable reason -> throwError reason
    Card.Playable -> pure $ case Vector.fromList cs of
      Nothing -> Playing $ TrickPlaying trick {cards = cs}
      Just vec ->
        Closed $
          TrickClosed trick {cards = rotate (negate trick.leader) vec}
  where
    cs = List.snoc trick.cards card

winner :: JassNat n => TrickClosed n -> Finite n
winner (TrickClosed trick) = Vector.maxIndexBy (Card.compare trick.variant lead) trick.cards
  where
    lead = Card.suit $ Vector.index trick.cards trick.leader

points :: TrickClosed n -> Int
points (TrickClosed trick) = Vector.sum $ Vector.map (Card.value trick.variant) trick.cards

rotate :: KnownNat n => Finite n -> Vector n a -> Vector n a
rotate n v = Vector.generate (\i -> Vector.index v $ i + n)

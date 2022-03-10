module Trick
  ( Trick,
    play,
    winner,
    points,
  )
where

import Card (Card, Cards)
import Card qualified
import Control.Monad.Except
import Control.Monad.State (MonadState, get, modify)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits (KnownNat)
import Jass
import Lens (over)
import List qualified
import Set qualified
import Variant (Variant)
import View qualified

data Trick n = Trick
  { variant :: Variant,
    leader :: Finite n,
    cards :: Vector n Card
  }
  deriving (Eq, Show)

play :: (MonadJass n m, MonadState (Vector n Cards) m) => Variant -> Finite n -> m (Trick n)
play variant leader = playCard []
  where
    playCard cards = maybe (go cards) (pure . close) $ Vector.fromList cards
    close cards = Trick {variant, leader, cards = rotate (negate leader) cards}
    go cards = do
      hands <- get
      let player = leader + fromIntegral (length cards)
          hand = Vector.index hands player
          view = View.playing leader variant hands cards
      card <- promptCard player view $
        \c -> case Card.status variant cards hand c of
          Card.Unplayable reason -> throwError reason
          Card.Playable -> pure c
      modify (over (Vector.ix player) (Set.delete card))
      playCard (List.snoc cards card)

winner :: JassNat n => Trick n -> Finite n
winner trick = Vector.maxIndexBy (Card.compare trick.variant lead) trick.cards
  where
    lead = Card.suit $ Vector.index trick.cards trick.leader

points :: Trick n -> Int
points trick = Vector.sum $ Vector.map (Card.value trick.variant) trick.cards

rotate :: KnownNat n => Finite n -> Vector n a -> Vector n a
rotate n v = Vector.generate (\i -> Vector.index v $ i + n)

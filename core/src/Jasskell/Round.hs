module Jasskell.Round
  ( Round,
    Interface (..),
    View (MakeView),
    tricks,
    play,
    rotate,
  )
where

import Data.Finite (Finite)
import Data.Type.Equality ((:~:) (Refl))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import GHC.TypeNats (Div)
import Jasskell.Card (Card, Cards)
import Jasskell.Card qualified as Card
import Jasskell.Jass (JassNat)
import Jasskell.Round.View (View (MakeView))
import Jasskell.Round.View qualified as View
import Jasskell.Trick (Trick)
import Jasskell.Trick qualified as Trick
import Jasskell.Variant (Variant)
import Jasskell.Variant qualified as Variant

newtype Round n = Round {tricks :: Vector (Div 36 n) (Trick n)}
  deriving (Eq, Show)

data Interface n m = Interface
  { promptCard :: Finite n -> View n -> m Card,
    throwBadCard :: forall a. Card.Reason -> m a
  }

play ::
  forall n m.
  (JassNat n, Monad m) =>
  Interface n m ->
  Variant ->
  Finite n ->
  Vector n Cards ->
  m (Round n)
play Interface {..} variant leader =
  evalStateT $ Round <$> Vector.constructM playTrick
  where
    playTrick ::
      forall i.
      KnownNat i =>
      Vector i (Trick n) ->
      StateT (Vector n Cards) m (Trick n)
    playTrick ts = Trick.play trickInterface var l
      where
        (var, l) = case Vector.notEmpty ts of
          Nothing -> (variant, leader)
          Just Refl ->
            let t = Vector.last ts
             in (Variant.next $ Trick.variant t, Trick.winner t)
        trickInterface =
          Trick.Interface
            { Trick.promptCard = prompt,
              Trick.throwBadCard = lift . throwBadCard
            }
        prompt current cards = do
          hands <- get
          let view =
                MakeView
                  { View.hands = hands,
                    View.tricks = Vector.toList ts,
                    View.variant = var,
                    View.leader = l,
                    View.cards = cards
                  }
          lift $ promptCard current view

rotate :: KnownNat n => Finite n -> Round n -> Round n
rotate i = coerce $ Vector.map (Trick.rotate i)

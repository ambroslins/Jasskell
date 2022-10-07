module Jasskell.Round
  ( Round,
    Interface (..),
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
import Jasskell.Card (BadCard, Card, Cards)
import Jasskell.Jass (JassNat)
import Jasskell.Round.View (RoundView)
import Jasskell.Round.View qualified as Round.View
import Jasskell.Trick (Hands, Trick)
import Jasskell.Trick qualified as Trick
import Jasskell.Variant (Variant)
import Jasskell.Variant qualified as Variant
import Jasskell.Views (Views)

newtype Round n = Round {tricks :: Vector (Div 36 n) (Trick n)}
  deriving (Eq, Show)

data Interface n m = Interface
  { promptCard :: Finite n -> Views RoundView n -> m Card,
    throwBadCard :: forall a. BadCard -> m a
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
  evalStateT (Round <$> Vector.constructM playTrick)
  where
    playTrick ::
      forall i.
      KnownNat i =>
      Vector i (Trick n) ->
      StateT (Hands n) m (Trick n)
    playTrick ts = Trick.play interface var l
      where
        (var, l) = case Vector.notEmpty ts of
          Nothing -> (variant, leader)
          Just Refl ->
            let lastTrick = Vector.last ts
             in (Variant.next (Trick.variant lastTrick), Trick.winner lastTrick)
        interface =
          Trick.Interface
            { Trick.promptCard = \current views ->
                lift $
                  promptCard
                    current
                    (Round.View.makeViews (Vector.toList ts) views),
              Trick.throwBadCard = lift . throwBadCard
            }

rotate :: KnownNat n => Finite n -> Round n -> Round n
rotate i = coerce (Vector.map (Trick.rotate i))

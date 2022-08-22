module Jasskell.Round
  ( Round,
    View (MakeView),
    tricks,
    play,
    rotate,
  )
where

import Control.Monad.Except (MonadError)
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

play ::
  forall n m.
  (JassNat n, MonadError Card.Reason m) =>
  (View n -> Finite n -> m Card) ->
  Variant ->
  Finite n ->
  Vector n Cards ->
  m (Round n)
play promptCard variant leader =
  evalStateT $ Round <$> Vector.constructM playTrick
  where
    playTrick ::
      forall i.
      KnownNat i =>
      Vector i (Trick n) ->
      StateT (Vector n Cards) m (Trick n)
    playTrick ts = Trick.play prompt var l
      where
        (var, l) = case Vector.notEmpty ts of
          Nothing -> (variant, leader)
          Just Refl ->
            let t = Vector.last ts
             in (Variant.next $ Trick.variant t, Trick.winner t)
        prompt :: [Card] -> Finite n -> StateT (Vector n Cards) m Card
        prompt cards current = do
          hs <- get
          let view =
                MakeView
                  { View.hands = hs,
                    View.tricks = Vector.toList ts,
                    View.variant = var,
                    View.leader = l,
                    View.cards = cards
                  }
          lift $ promptCard view current

rotate :: KnownNat n => Finite n -> Round n -> Round n
rotate i = coerce $ Vector.map (Trick.rotate i)

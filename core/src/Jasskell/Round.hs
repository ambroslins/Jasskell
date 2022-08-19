module Jasskell.Round
  ( Round,
    tricks,
    play,
  )
where

import Control.Monad.Free (MonadFree)
import Data.Finite (Finite)
import Data.Type.Equality ((:~:) (Refl))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import GHC.TypeNats (Div)
import Jasskell.Card (Cards)
import Jasskell.Jass (JassNat)
import Jasskell.Prompt (Prompt)
import Jasskell.Prompt qualified as Prompt
import Jasskell.Round.View (View)
import Jasskell.Round.View qualified as View
import Jasskell.Trick (Trick)
import Jasskell.Trick qualified as Trick
import Jasskell.Variant (Variant)
import Jasskell.Variant qualified as Variant

newtype Round n = Round {tricks :: Vector (Div 36 n) (Trick n)}
  deriving (Eq, Show)

play ::
  forall n m.
  (JassNat n, MonadFree (Prompt (View n)) m) =>
  Finite n ->
  Vector n Cards ->
  m (Round n)
play leader hands = do
  variant <- chooseVariant
  let playTrick ::
        forall i.
        KnownNat i =>
        Vector i (Trick n) ->
        StateT (Vector n Cards) m (Trick n)
      playTrick ts =
        let adapt = Prompt.mapView $ View.fromTrick $ Vector.toList ts
         in adapt $ case Vector.notEmpty ts of
              Nothing -> Trick.play variant leader
              Just Refl ->
                let t = Vector.last ts
                 in Trick.play
                      (Variant.next $ Trick.variant t)
                      (Trick.winner t)
  Round <$> evalStateT (Vector.constructM playTrick) hands
  where
    chooseVariant :: m Variant
    chooseVariant = Prompt.variant view
      where
        view =
          View.MakeView
            { View.hands = hands,
              View.table = Vector.replicate Nothing,
              View.leader = leader,
              View.variant = Nothing,
              View.tricks = []
            }

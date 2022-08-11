module Jasskell.Round
  ( Round,
    tricks,
    play,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized.Extra qualified as Vector
import GHC.TypeNats (Div)
import Jasskell.Card (Cards)
import Jasskell.Jass (MonadJass, promptVariant)
import Jasskell.Trick (Trick)
import Jasskell.Trick qualified as Trick
import Jasskell.Variant qualified as Variant
import Jasskell.View qualified as View

newtype Round n = Round {tricks :: Vector (Div 36 n) (Trick n)}
  deriving (Show)

play :: (MonadJass n m) => Finite n -> Vector n Cards -> m (Round n)
play leader = evalStateT $ do
  hands <- get
  let views = View.makeDeclaring hands leader
  variant <- promptVariant views
  firstTrick <- Trick.play variant leader
  let playTrick t = Trick.play (Variant.next $ Trick.variant t) (Trick.winner t)
  Round <$> Vector.iterateM playTrick firstTrick

module Round
  ( Round,
    tricks,
    play,
  )
where

import Card (Cards)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized.Extra qualified as Vector
import GHC.TypeNats (Div)
import Jass (MonadJass, promptVariant)
import Trick (Trick)
import Trick qualified
import Variant qualified
import View.Declaring qualified

newtype Round n = Round {tricks :: Vector (Div 36 n) (Trick n)}
  deriving (Show)

play :: (MonadJass n m) => Finite n -> Vector n Cards -> m (Round n)
play leader = evalStateT $ do
  hands <- get
  let views = View.Declaring.make hands leader
  variant <- promptVariant views
  firstTrick <- Trick.play variant leader
  let playTrick t = Trick.play (Variant.next $ Trick.variant t) (Trick.winner t)
  Round <$> Vector.iterateM playTrick firstTrick

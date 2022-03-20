module Round
  ( Round,
    play,
  )
where

import Card (Cards)
import Control.Monad.State (MonadState, evalStateT, get)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits (Div)
import Jass
import Trick (Trick)
import Trick qualified
import Variant (Variant)
import Variant qualified
import View qualified

newtype Round n = Round (Vector (Div 36 n) (Trick n))
  deriving (Show)

play :: MonadJass n m => Vector n Cards -> Finite n -> m (Round n)
play hands leader = evalStateT (playRound leader) hands

playRound :: forall n m. (MonadJass n m, MonadState (Vector n Cards) m) => Finite n -> m (Round n)
playRound roundLeader = do
  hands <- get
  let view = View.makeDeclaring roundLeader hands
  variant <- promptVariant view
  playTricks [] variant roundLeader
  where
    playTricks :: [Trick n] -> Variant -> Finite n -> m (Round n)
    playTricks tricks variant leader = maybe (go tricks variant leader) (pure . Round . Vector.reverse) $ Vector.fromList tricks
    go tricks variant leader = do
      trick <- Trick.play variant leader
      playTricks (trick : tricks) (Variant.next variant) (Trick.winner trick)

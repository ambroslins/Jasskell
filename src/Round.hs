module Round
  ( Round,
    RoundStarting,
    RoundPlaying,
    RoundFinished,
    current,
    playCard,
    chooseVariant,
  )
where

import Card (Card, Cards)
import Card qualified
import Control.Monad.Except
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits (Div, KnownNat)
import JassNat (JassNat)
import Lens (over)
import Set qualified
import Trick (TrickClosed, TrickPlaying)
import Trick qualified
import Variant (Variant)
import Prelude hiding (round)

data Round n
  = Starting (RoundStarting n)
  | Playing (RoundPlaying n)
  | Finished (RoundFinished n)
  deriving (Show)

data RoundStarting n = RoundStarting
  { hands :: Vector n Cards,
    leader :: Finite n
  }
  deriving (Show)

data RoundPlaying n = RoundPlaying
  { hands :: Vector n Cards,
    trick :: TrickPlaying n,
    tricks :: [TrickClosed n]
  }
  deriving (Show)

newtype RoundFinished n = RoundFinished (Vector (Div 36 n) (TrickClosed n))
  deriving newtype (Eq, Show)

current :: KnownNat n => RoundPlaying n -> Finite n
current round = Trick.current round.trick

playCard :: (MonadError Card.Reason m, JassNat n) => Card -> RoundPlaying n -> m (Round n)
playCard card round =
  Trick.playCard hand card round.trick <&> \case
    Trick.Playing trick ->
      Playing
        round
          { hands = hands,
            trick = trick
          }
    Trick.Closed trick ->
      let tricks = trick : round.tricks
          playing =
            ( Playing
                round
                  { hands = hands,
                    trick = Trick.next trick,
                    tricks = tricks
                  }
            )
       in maybe
            playing
            (Finished . RoundFinished . Vector.reverse)
            (Vector.fromList tricks)
  where
    ix = current round
    hand = Vector.index round.hands ix
    hands = over (Vector.ix ix) (Set.delete card) round.hands

chooseVariant :: Variant -> RoundStarting n -> RoundPlaying n
chooseVariant variant round =
  RoundPlaying
    { hands = round.hands,
      trick = Trick.new variant round.leader,
      tricks = []
    }
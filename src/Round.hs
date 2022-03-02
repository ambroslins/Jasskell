module Round
  ( Round,
    Record,
    current,
    playCard,
    chooseVariant,
  )
where

import Card (Card, Cards)
import Card qualified
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits (Div, KnownNat)
import JassNat (JassNat)
import Lens (over)
import Round.Record (Record)
import Set qualified
import Trick (TrickClosed, TrickPlaying)
import Trick qualified
import Variant (Variant)
import Prelude hiding (round)

data Round n
  = Starting (RoundStarting n)
  | Playing (RoundPlaying n)
  | Finished (Vector (Div 36 n) (TrickClosed n))
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

current :: KnownNat n => RoundPlaying n -> Finite n
current round = Trick.current round.trick

playCard :: JassNat n => Card -> RoundPlaying n -> Either Card.Reason (Round n)
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
            (Finished . Vector.reverse)
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
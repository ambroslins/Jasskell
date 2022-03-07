module Round
  ( Starting,
    Playing,
    Finished,
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
import Trick qualified
import Variant (Variant)
import Prelude hiding (round)

data Starting n = Starting
  { hands :: Vector n Cards,
    leader :: Finite n
  }
  deriving (Show)

data Playing n = Playing
  { hands :: Vector n Cards,
    trick :: Trick.Playing n,
    tricks :: [Trick.Closed n]
  }
  deriving (Show)

newtype Finished n = Finished (Vector (Div 36 n) (Trick.Closed n))
  deriving newtype (Eq, Show)

current :: KnownNat n => Playing n -> Finite n
current round = Trick.current round.trick

playCard :: (MonadError Card.Reason m, JassNat n) => Card -> Playing n -> m (Either (Playing n) (Finished n))
playCard card round =
  Trick.playCard hand card round.trick <&> \case
    Left trickPlaying ->
      Left
        round
          { hands = hands,
            trick = trickPlaying
          }
    Right trickClosed ->
      let tricks = trickClosed : round.tricks
          playing =
            ( Left
                round
                  { hands = hands,
                    trick = Trick.next trickClosed,
                    tricks = tricks
                  }
            )
       in maybe
            playing
            (Right . Finished . Vector.reverse)
            (Vector.fromList tricks)
  where
    ix = current round
    hand = Vector.index round.hands ix
    hands = over (Vector.ix ix) (Set.delete card) round.hands

chooseVariant :: Variant -> Starting n -> Playing n
chooseVariant variant round =
  Playing
    { hands = round.hands,
      trick = Trick.new variant round.leader,
      tricks = []
    }
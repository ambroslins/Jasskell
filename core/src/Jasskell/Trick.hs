module Jasskell.Trick
  ( Trick,
    play,
    variant,
    leader,
    cards,
    winner,
    points,
  )
where

import Control.Monad.Free (MonadFree)
import Data.Finite (Finite)
import Data.Set qualified as Set
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import GHC.TypeNats (type (+))
import Jasskell.Card (Card, Cards)
import Jasskell.Card qualified as Card
import Jasskell.Prompt (Prompt)
import Jasskell.Prompt qualified as Prompt
import Jasskell.Trick.View (View)
import Jasskell.Trick.View qualified as View
import Jasskell.Variant (Variant)
import Relude.Extra.Lens (over)

data Trick n = Trick
  { variant :: Variant,
    leader :: Finite n,
    cards :: Vector n Card
  }
  deriving (Eq, Show)

play ::
  forall n m.
  (KnownNat n, MonadFree (Prompt (View n)) m, MonadState (Vector n Cards) m) =>
  Variant ->
  Finite n ->
  m (Trick n)
play variant leader = close <$> Vector.constructM playCard
  where
    close cs = Trick {variant, leader, cards = Vector.rotate (negate leader) cs}
    playCard :: forall i. KnownNat i => Vector i Card -> m Card
    playCard table = do
      hands <- get
      let current = leader + fromIntegral (Vector.length table)
          cards =
            Vector.rotate (negate leader) $
              Vector.unfoldrN
                (\case [] -> (Nothing, []); c : cs -> (Just c, cs))
                (Vector.toList table)
          view =
            View.MakeView
              { View.hands = hands,
                View.table = cards,
                View.leader = leader,
                View.variant = variant
              }

      card <- Prompt.card view
      modify $ over (Vector.ix current) (Set.delete card)
      pure card

winner :: n ~ (m + 1) => Trick n -> Finite n
winner Trick {leader, cards, variant} =
  Vector.maxIndexBy (Card.compare variant lead) cards
  where
    lead = Card.suit $ Vector.index cards leader

points :: Trick n -> Int
points Trick {cards, variant} = sum $ Vector.map (Card.value variant) cards

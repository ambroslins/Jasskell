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

import Control.Monad.Except (MonadError (throwError), runExcept)
import Data.Finite (Finite)
import Data.Set qualified as Set
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import GHC.TypeNats (type (+))
import Jasskell.Card (Card, Cards)
import Jasskell.Card qualified as Card
import Jasskell.Variant (Variant)
import Relude.Extra.Lens qualified as Lens

data Trick n = Trick
  { variant :: Variant,
    leader :: Finite n,
    cards :: Vector n Card
  }
  deriving (Eq, Show)

play ::
  forall n m.
  (KnownNat n, MonadState (Vector n Cards) m, MonadError Card.Reason m) =>
  ([Card] -> Finite n -> m Card) ->
  Variant ->
  Finite n ->
  m (Trick n)
play promptCard variant leader = close <$> Vector.constructM playCard
  where
    close cs = Trick {variant, leader, cards = Vector.rotate (negate leader) cs}
    playCard :: forall i. KnownNat i => Vector i Card -> m Card
    playCard cards = do
      let current = leader + fromIntegral (Vector.length cards)
          cardList = Vector.toList cards
      hand <- gets (`Vector.index` current)
      card <- promptCard cardList current
      case runExcept $ Card.playable variant cardList hand card of
        Left reason -> throwError reason
        Right newHand -> do
          modify $ Lens.set (Vector.ix current) newHand
          pure card

winner :: n ~ (m + 1) => Trick n -> Finite n
winner Trick {leader, cards, variant} =
  Vector.maxIndexBy (Card.compare variant lead) cards
  where
    lead = Card.suit $ Vector.index cards leader

points :: Trick n -> Int
points Trick {cards, variant} = sum $ Vector.map (Card.value variant) cards

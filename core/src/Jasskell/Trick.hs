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

import Data.Finite (Finite)
import Data.Set qualified as Set
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import Jasskell.Card (Card, Cards)
import Jasskell.Card qualified as Card
import Jasskell.Jass (JassNat, MonadJass, promptCard)
import Jasskell.Variant (Variant)
import Jasskell.View qualified as View
import Relude.Extra.Lens (over)

data Trick n = Trick
  { variant :: Variant,
    leader :: Finite n,
    cards :: Vector n Card
  }
  deriving (Eq, Show)

play :: (MonadJass n m, MonadState (Vector n Cards) m) => Variant -> Finite n -> m (Trick n)
play variant leader = close <$> Vector.unfoldrM playCard []
  where
    close cs = Trick {variant, leader, cards = Vector.rotate (negate leader) cs}
    playCard cs = do
      hands <- get
      let current = leader + fromIntegral (length cs)
          Just table = Vector.fromListN $ map Just cs ++ repeat Nothing
          views = View.makePlaying hands variant (Vector.rotate leader table) leader
      card <- promptCard views
      modify $ over (Vector.ix current) (Set.delete card)
      pure (card, cs ++ [card])

winner :: JassNat n => Trick n -> Finite n
winner Trick {leader, cards, variant} = Vector.maxIndexBy (Card.compare variant lead) cards
  where
    lead = Card.suit $ Vector.index cards leader

points :: Trick n -> Int
points Trick {cards, variant} = sum $ Vector.map (Card.value variant) cards

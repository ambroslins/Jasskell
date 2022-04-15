module Game where

import Card (Suit (Bells))
import Card.Valid qualified as Card
import Control.Monad.Free (Free, iterM)
import Data.Set qualified as Set
import Jass (Jass (..), MonadJass, deal)
import Round (Round)
import Round qualified
import System.Random (RandomGen)
import Variant (Variant (Trump))
import View.Playing qualified

newtype Game n = Game {rounds :: NonEmpty (Round n)}
  deriving (Show)

play :: (MonadJass n m, RandomGen g) => g -> m (Game n)
play = evalStateT $ do
  hands <- state deal
  r <- Round.play 0 hands
  pure $ Game (r :| [])

run :: KnownNat n => Free (Jass n) (Game n) -> IO (Game n)
run = iterM $ \case
  PromptCard views next ->
    let current = View.Playing.current (views 0)
        valids = Card.valids $ views current
     in next (Set.elemAt 0 valids)
  PromptVariant _ next -> next $ Trump Bells
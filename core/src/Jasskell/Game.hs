module Jasskell.Game where

import Control.Monad.Free (Free, iterM)
import Data.Set qualified as Set
import Jasskell.Card (Suit (Bells))
import Jasskell.Jass (Jass (..), MonadJass, deal)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Variant (Variant (Trump))
import Jasskell.View.Playing qualified as View.Playing
import System.Random (RandomGen)

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
        valids = View.Playing.playableCards $ views current
     in next (Set.elemAt 0 valids)
  PromptVariant _ next -> next $ Trump Bells

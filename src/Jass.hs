{-# LANGUAGE TemplateHaskell #-}

module Jass where

import Card (Card (..), Cards, Rank (..), Suit (..), deck)
import Control.Exception (assert)
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Control.Monad.ST (runST)
import Data.Finite (Finite, finite, getFinite)
import Data.Set qualified as Set
import Data.Vector.Mutable.Sized qualified as Vector.Mutable
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits (Div, type (+), type (-))
import System.Random (RandomGen, uniformR)
import Variant (Variant)
import View (Declaring, PlayableCard, Playing, Views)

data Jass n next
  = PromptCard (Views Playing n) (PlayableCard -> next)
  | PromptVariant (Views Declaring n) (Variant -> next)
  deriving (Functor)

makeFree ''Jass

type MonadJass n m = (JassNat n, MonadFree (Jass n) m)

class (KnownNat n, KnownNat (Div 36 n), n ~ ((n - 1) + 1)) => JassNat n where
  deal :: RandomGen g => g -> (Vector n Cards, g)
  deal = dealDeck deck

instance JassNat 3

instance JassNat 4

instance JassNat 5 where
  deal = dealDeck (Set.delete (Card Bells Six) deck)

instance JassNat 6

instance JassNat 7 where
  deal = dealDeck (Set.delete (Card Bells Six) deck)

dealDeck :: (KnownNat n, RandomGen g) => Cards -> g -> (Vector n Cards, g)
dealDeck cards gen = Vector.withSizedList (toList cards) $ \cs ->
  let (shuffled, g) = shuffle gen cs
      (n, rest) = Vector.length cs `divMod` Vector.length decks
      decks = Vector.unfoldrN (splitAt n) (Vector.toList shuffled)
   in assert (rest == 0) (Vector.map Set.fromList decks, g)

shuffle :: (KnownNat n, RandomGen g) => g -> Vector n a -> (Vector n a, g)
shuffle gen xs = runST $ do
  ys <- Vector.thaw xs
  let randomSwap g i _ =
        let (j, g') = uniformFiniteR (i, maxBound) g
         in Vector.Mutable.swap ys i j $> g'
  gen' <- Vector.ifoldM' randomSwap gen xs
  zs <- Vector.freeze ys
  pure (zs, gen')

uniformFiniteR :: (KnownNat n, RandomGen g) => (Finite n, Finite n) -> g -> (Finite n, g)
uniformFiniteR (low, high) gen =
  let (i, gen') = uniformR (getFinite low, getFinite high) gen
   in (finite i, gen')
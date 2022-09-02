module Jasskell.Dealer
  ( Dealer,
    run,
    four,
  )
where

import Control.Exception (assert)
import Control.Monad.ST (runST)
import Data.Finite (Finite, finite, getFinite)
import Data.Set qualified as Set
import Data.Vector.Mutable.Sized qualified as Vector.Mutable
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Card (Cards, deck)
import System.Random (RandomGen, uniformR)

newtype Dealer n = Dealer
  {run :: forall g. (KnownNat n, RandomGen g) => g -> (Vector n Cards, g)}

four :: Dealer 4
four = Dealer $ dealDeck deck

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

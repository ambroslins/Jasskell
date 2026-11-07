module Jasskell.Dealer
  ( Dealer,
    four,
    shuffle,
  )
where

import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.Bifunctor (first)
import Data.Fin qualified as Fin
import Data.Functor (($>))
import Data.Vec (Vec)
import Data.Vec qualified as Vec
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Jasskell.Card (Card)
import Jasskell.Card.Set (CardSet)
import Jasskell.Card.Set qualified as CardSet
import System.Random.Stateful (RandomGen)
import System.Random.Stateful qualified as Random

type Dealer n = forall g. (RandomGen g) => g -> (Vec n CardSet, g)

four :: Dealer 4
four = first split . shuffle CardSet.deck
  where
    split v = Vec.generate $ \i ->
      VU.foldl' (flip CardSet.insert) CardSet.empty $
        VU.slice (Fin.toInt i * 9) 9 v

shuffle :: forall g. (RandomGen g) => CardSet -> g -> (VU.Vector Card, g)
shuffle deck gen = runST $ do
  mv <- VUM.new size
  CardSet.foldM_ (\i c -> VUM.write mv i c $> i + 1) 0 deck
  let go g i =
        let (j, g') = Random.uniformR (i, size - 1) g
         in VUM.swap mv i j $> g'
  g <- foldM go gen [0 .. size - 2]
  v <- VU.unsafeFreeze mv
  pure (v, g)
  where
    size = CardSet.size deck

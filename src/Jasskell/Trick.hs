module Jasskell.Trick where

import           Data.Finite
import qualified Data.Vector.Sized             as Vector
import           Data.Vector.Sized              ( Vector )
import           Jasskell.Card
import           Jasskell.Card.Suit
import           GHC.TypeLits

data Resolved
data Unresolved

data TrickUnresolved n = TrickUnresolved (Finite n) [Card] deriving Show

data TrickResolved n = TrickResolved Suit (Vector n Card) deriving Show

data Trick n = Unresolved (TrickUnresolved n)
                   | Resolved (TrickResolved n)
                   deriving Show

addCard :: KnownNat n => Card -> TrickUnresolved n -> Trick n
addCard c (TrickUnresolved f cs) = case Vector.fromListN t of
  Just v  -> Resolved $ TrickResolved (suit $ head cs') v
  Nothing -> Unresolved $ TrickUnresolved f cs'
 where
  cs' = cs ++ [c]
  t   = (drop <> take) (fromInteger $ getFinite f) cs'

newTrick :: Finite n -> TrickUnresolved n
newTrick f = TrickUnresolved f []

{-# LANGUAGE ScopedTypeVariables #-}

module Jasskell.Trick where

import           Data.Finite
import qualified Data.Vector.Sized             as Vector
import           Data.Vector.Sized              ( Vector )
import           Jasskell.Card
import           GHC.TypeLits

data Resolved
data Unresolved

data TrickUnresolved n = TrickUnresolved (Finite n) [Card] deriving Show

data TrickResolved n = TrickResolved (Finite n) (Vector n Card) deriving Show

data Trick n = Unresolved (TrickUnresolved n)
                   | Resolved (TrickResolved n)
                   deriving Show

addCard :: KnownNat n => Card -> TrickUnresolved n -> Trick n
addCard c (TrickUnresolved f cs) = case Vector.fromListN cs' of
  Just v  -> Resolved $ TrickResolved f $ rotateN (-toInteger f) v
  Nothing -> Unresolved $ TrickUnresolved f cs'
  where cs' = cs ++ [c]

newTrick :: Finite n -> TrickUnresolved n
newTrick f = TrickUnresolved f []

rotateN :: (KnownNat n) => Integer -> Vector n a -> Vector n a
rotateN n vec = Vector.generate (Vector.index vec . (+ modulo n))

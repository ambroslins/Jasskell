module Jasskell.Trick
    ( Trick(..)
    , TrickUnresolved
    , TrickResolved
    , addCard
    , playedCard
    , newTrick
    , winner
    , points
    )
where

import           Data.Foldable                  ( maximumBy )
import           Data.Finite
import           Data.Function                  ( on )
import qualified Data.Vector.Sized             as Vector
import           Data.Vector.Sized              ( Vector
                                                , index
                                                , indexed
                                                )
import           Jasskell.Card
import           Jasskell.Variant
import           GHC.TypeLits

data TrickUnresolved n = TrickUnresolved (Finite n) [Card]

data TrickResolved n = TrickResolved (Finite n) (Vector n Card)

data Trick n = Unresolved (TrickUnresolved n)
             | Resolved (TrickResolved n)

addCard :: KnownNat n => Card -> TrickUnresolved n -> Trick n
addCard c (TrickUnresolved f cs) = case Vector.fromListN cs' of
    Just v  -> Resolved $ TrickResolved f $ rotateN (-toInteger f) v
    Nothing -> Unresolved $ TrickUnresolved f cs'
    where cs' = cs ++ [c]

playedCard :: KnownNat n => Finite n -> Trick n -> Maybe Card
playedCard i (Unresolved (TrickUnresolved f cs)) =
    lookup i $ zip (iterate (+ 1) f) cs
playedCard i (Resolved (TrickResolved _ v)) = Just $ index v i

newTrick :: Finite n -> TrickUnresolved n
newTrick f = TrickUnresolved f []

rotateN :: (KnownNat n) => Integer -> Vector n a -> Vector n a
rotateN n vec = Vector.generate (index vec . (+ modulo n))

winner :: Variant -> TrickResolved n -> Finite n
winner var (TrickResolved f v) =
    fst $ maximumBy (compareCard var (suit $ index v f) `on` snd) $ indexed v

points :: Variant -> TrickResolved n -> Int
points var (TrickResolved _ cs) = sum $ fmap (value var) cs


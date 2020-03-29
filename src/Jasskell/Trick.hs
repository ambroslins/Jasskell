{-# LANGUAGE ScopedTypeVariables #-}

module Jasskell.Trick where

import           Data.List                      ( maximumBy )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Finite
import           Data.Function                  ( on )
import qualified Data.Vector.Sized             as Vector
import           Data.Vector.Sized              ( Vector
                                                , index
                                                , indexed
                                                )
import qualified Data.Set                      as Set
import           Jasskell.Card
import           Jasskell.Card.Suit
import           Jasskell.Variant
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

highestCard :: Variant -> Suit -> NonEmpty Card -> Card
highestCard var lead = maximumBy (compareCard var lead)

playableCards :: Variant -> [Card] -> Cards -> Cards
playableCards _   []       hand = hand
playableCards var (c : cs) hand = case var of
    Trump trump -> if lead == trump
        then if Set.null $ Set.delete (puur trump) $ follows trump
            then hand
            else follows trump
        else
            let followAndTrump = Set.union (follows trump) followerOrAll
            in  if suit highest == trump
                    then Set.filter
                        (\x -> suit x /= trump || cardGT var lead x highest)
                        followAndTrump
                    else followAndTrump
    _ -> followerOrAll
  where
    lead = suit c
    follows s = Set.filter ((== s) . suit) hand
    highest       = highestCard var lead (c :| cs)
    followerOrAll = if Set.null $ follows lead then hand else follows lead

module Jasskell.Meld
  ( Meld (..),
    melds,
    cards,
    points,
    compare,
    betterOn,
  )
where

import Data.Ord (comparing)
import Jasskell.Card (Card, Rank (..))
import Jasskell.Card qualified as Card
import Jasskell.Card.Set (CardSet)
import Jasskell.Card.Set qualified as CardSet
import Jasskell.Variant (Variant (Trump))
import Prelude hiding (compare)

data Meld
  = -- | High card and length (at least 3)
    Sequence Card Int
  | Quartet Rank
  deriving (Eq, Show)

melds :: Variant -> CardSet -> [Meld]
melds v cs = case candidates of
  [] -> []
  m : ms ->
    let !best = foldl' (betterOn id v) m ms
     in best : melds v (cs `CardSet.difference` cards best)
  where
    candidates =
      map Quartet (CardSet.quartets cs)
        <> map (\s -> Sequence s.high s.length) (CardSet.sequences cs)

cards :: Meld -> CardSet
cards = \case
  Sequence high len ->
    let suit = Card.suit high
        go i r cs
          | i >= len = cs
          | otherwise = go (i + 1) (pred r) (CardSet.insert (Card.make suit r) cs)
     in go 0 (Card.rank high) CardSet.empty
  Quartet rank -> CardSet.ofRank rank

points :: Meld -> Int
points = \case
  Sequence _ len -> case len of
    3 -> 20
    4 -> 50
    5 -> 100
    6 -> 120
    7 -> 140
    8 -> 160
    9 -> 180
    _ -> error $ "Jasskell.Meld.points: invalid sequence length: " <> show len
  Quartet rank -> case rank of
    Nine -> 150
    Ten -> 100
    Under -> 200
    Over -> 100
    King -> 100
    Ace -> 100
    _ -> error $ "Jasskell.Meld.points: invalid quartet rank: " <> show rank

compare :: Variant -> Meld -> Meld -> Ordering
compare variant =
  comparing points
    <> comparing size -- Sequence of 5 beats quartet
    <> comparing rank
    <> comparing trump
  where
    size = \case
      Sequence _ len -> len
      Quartet _ -> 4
    rank = \case
      Sequence high _ -> Card.rank high
      Quartet r -> r
    trump = case variant of
      Trump t -> \case
        Sequence high _ -> Card.suit high == t
        Quartet _ -> False
      _ -> const False

betterOn :: (a -> Meld) -> Variant -> a -> a -> a
betterOn f v a b
  | compare v (f a) (f b) == LT = b
  | otherwise = a
{-# INLINE betterOn #-}

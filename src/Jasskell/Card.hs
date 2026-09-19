module Jasskell.Card
  ( Card,
    Suit (..),
    Rank (..),
    suit,
    rank,
    make,
    weli,
    compare,
    max,
    points,
    abbreviation,
  )
where

import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Jasskell.Card.Internal
import Jasskell.Variant (Direction (..), Variant (..))
import Prelude hiding (compare, filter, max, null)

weli :: Card
weli = make Bells Six

compare :: Suit -> Variant -> Card -> Card -> Ordering
compare lead variant = case variant of
  Trump trump ->
    let puur = make trump Under
        nell = make trump Nine
     in comparing (== puur)
          <> comparing (== nell)
          <> comparing (\c -> suit c == trump)
          <> compareDirection TopDown
  Direction d -> compareDirection d
  Slalom d -> compareDirection d
  where
    compareDirection d =
      comparing (\c -> suit c == lead)
        <> case d of
          TopDown -> comparing rank
          BottomUp -> comparing (Down . rank)

max :: Suit -> Variant -> Card -> Card -> Card
max lead variant c1 c2 = case compare lead variant c1 c2 of
  LT -> c2
  _ -> c1

points :: Variant -> Card -> Int
points v c = case v of
  Trump trump
    | c == make trump Under -> 20
    | c == make trump Nine -> 14
  Direction _ | rank c == Eight -> 8
  Slalom _ | rank c == Eight -> 8
  _ -> case rank c of
    Six -> 0
    Seven -> 0
    Eight -> 0
    Nine -> 0
    Ten -> 10
    Under -> 2
    Over -> 3
    King -> 4
    Ace -> 11

abbreviation :: Card -> Text
abbreviation c = s <> r
  where
    s = case suit c of
      Bells -> "B"
      Hearts -> "H"
      Acorns -> "A"
      Leaves -> "L"
    r = case rank c of
      Six -> "6"
      Seven -> "7"
      Eight -> "8"
      Nine -> "9"
      Ten -> "10"
      Under -> "U"
      Over -> "O"
      King -> "K"
      Ace -> "A"

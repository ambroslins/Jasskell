module Card
  ( Card (..),
    Suit (..),
    Rank (..),
    Status (..),
    Cards,
    deck,
    value,
    compare,
    status,
    isPlayable,
  )
where

import Card.Suit
import Data.Foldable (maximumBy)
import Data.Set qualified as Set
import Variant
import Prelude hiding (compare)

data Rank = Six | Seven | Eight | Nine | Ten | Under | Over | King | Ace
  deriving (Eq, Ord, Bounded, Enum, Show)

data Card = Card {suit :: Suit, rank :: Rank}
  deriving (Eq, Ord, Show)

type Cards = Set Card

deck :: Cards
deck = Set.fromList $ Card <$> universe <*> universe

value :: Variant -> Card -> Int
value variant card = case rank card of
  Six -> 0
  Seven -> 0
  Eight -> case variant of
    Trump _ -> 0
    Direction _ -> 8
    Slalom _ -> 8
  Nine -> if isTrump then 14 else 0
  Ten -> 10
  Under -> if isTrump then 20 else 2
  Over -> 3
  King -> 4
  Ace -> 11
  where
    isTrump = variant == Trump (suit card)

compare :: Variant -> Suit -> Card -> Card -> Ordering
compare variant lead = case variant of
  Trump trump ->
    comparing (== puur)
      <> comparing (== nell)
      <> comparing ((== trump) . suit)
      <> compareDirection TopDown
    where
      puur = Card trump Under
      nell = Card trump Nine
  Direction dir -> compareDirection dir
  Slalom dir -> compareDirection dir
  where
    compareDirection :: Direction -> Card -> Card -> Ordering
    compareDirection dir =
      comparing ((== lead) . suit)
        <> ( case dir of
               TopDown -> comparing rank
               BottomUp -> comparing (Down . rank)
           )
        <> comparing suit

data Status
  = Playable
  | NotInHand
  | FollowTrump Suit
  | FollowLead Suit
  | Undertrump Card
  deriving (Eq, Show)

status :: Variant -> [Card] -> Cards -> Card -> Status
status variant table hand card =
  if Set.notMember card hand
    then NotInHand
    else case table of
      [] -> Playable
      (c : cs) -> case variant of
        Trump trump
          | lead == trump ->
            check (FollowTrump trump) $
              suit card == trump || null (Set.delete puur trumps)
          | suit highest == trump ->
            check (Undertrump highest) $
              suit card /= trump
                || comp card highest == GT
                || (null highers && null (Set.difference hand trumps))
          | otherwise ->
            check (FollowLead lead) $
              suit card `elem` [lead, trump] || null followers
          where
            trumps = Set.filter ((== trump) . suit) hand
            highest = maximumBy comp (c :| cs)
            highers = Set.filter (\x -> comp x highest == GT) hand
            puur = Card trump Under
        _ ->
          check (FollowLead lead) $
            suit card == lead || null followers
        where
          lead = suit c
          followers = Set.filter ((== lead) . suit) hand
          comp = compare variant lead
          check r p = if p then Playable else r

isPlayable :: Variant -> [Card] -> Cards -> Card -> Bool
isPlayable variant table hand card = status variant table hand card == Playable
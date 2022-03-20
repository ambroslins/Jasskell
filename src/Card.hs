module Card
  ( Card (Card),
    Suit (..),
    Rank (..),
    Playable (card),
    Reason (..),
    Cards,
    rank,
    suit,
    deck,
    value,
    compare,
    validate,
  )
where

import Card.Suit
import List qualified
import Set qualified
import Variant
import Prelude hiding (compare)

data Rank = Six | Seven | Eight | Nine | Ten | Under | Over | King | Ace
  deriving (Eq, Ord, Bounded, Enum, Show)

data Card = Card {suit :: Suit, rank :: Rank}
  deriving (Eq, Ord, Show)

suit :: Card -> Suit
suit card = card.suit

rank :: Card -> Rank
rank card = card.rank

type Cards = Set Card

deck :: Cards
deck = Set.fromList $ Card <$> [minBound .. maxBound] <*> [minBound .. maxBound]

value :: Variant -> Card -> Int
value variant card = case card.rank of
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
    isTrump = variant == Trump card.suit

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

newtype Playable = Playable {card :: Card}
  deriving (Eq, Ord, Show)

data Reason
  = NotInHand
  | NotYourTurn
  | FollowTrump Suit
  | FollowLead Suit
  | Undertrump Card
  deriving (Eq, Show)

validate :: Variant -> [Card] -> Cards -> Card -> Either Reason Playable
validate variant table hand card =
  if Set.notMember card hand
    then Left $ NotInHand
    else case table of
      [] -> Right $ Playable card
      (c : cs) -> case variant of
        Trump trump
          | lead == trump ->
              check (FollowTrump trump) $
                card.suit == trump || Set.null (Set.delete puur trumps)
          | highest.suit == trump ->
              if card.suit == trump
                then
                  check (Undertrump highest) $
                    comp card highest == GT
                      || (Set.null highers && Set.isSubsetOf hand trumps)
                else
                  check (FollowLead lead) $
                    card.suit == lead || Set.null followers
          | otherwise ->
              check (FollowLead lead) $
                card.suit `List.elem` [lead, trump] || Set.null followers
          where
            trumps = Set.filter ((== trump) . suit) hand
            highest = maximumBy comp (c :| cs)
            highers = Set.filter (\x -> comp x highest == GT) hand
            puur = Card trump Under
        _ ->
          check (FollowLead lead) $
            card.suit == lead || Set.null followers
        where
          lead = c.suit
          followers = Set.filter ((== lead) . suit) hand
          comp = compare variant lead
          check r p = if p then Right $ Playable card else Left r

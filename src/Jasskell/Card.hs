module Jasskell.Card
    ( Card
    , Cards
    , suit
    , rank
    , isPuur
    , isNell
    , allCards
    , compareCard
    , highestCard
    , playableCards
    )
where

import           Data.Foldable                  ( maximumBy )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Jasskell.Card.Suit
import           Jasskell.Card.Rank
import           Jasskell.Variant
import           Text.Read

data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Ord)

instance Show Card where
    show c = show (suit c) ++ " " ++ show (rank c)

instance Read Card where
    readPrec = Card <$> readPrec <*> readPrec

type Cards = Set Card

isPuur :: Suit -> Card -> Bool
isPuur t c = suit c == t && rank c == Under

isNell :: Suit -> Card -> Bool
isNell t c = suit c == t && rank c == Nine

allCards :: Cards
allCards = Set.fromList
    [ Card { suit = s, rank = r } | s <- [Bells .. Leaves], r <- [Six .. Ace] ]

compareCard :: Variant -> Suit -> Card -> Card -> Ordering
compareCard var lead c1 c2 | c1 == c2              = EQ
                           | cardGE var lead c1 c2 = GT
                           | otherwise             = LT

cardGE :: Variant -> Suit -> Card -> Card -> Bool
cardGE (Trump trump) lead c1 c2 = if suit c1 == trump
    then
        (suit c2 /= trump)
        || (isPuur trump c1)
        || (isNell trump c1 && not (isPuur trump c2))
        || (rank c1 > rank c2)
    else suit c2 /= trump && cardGE (Direction TopDown) lead c1 c2
cardGE (Slalom dir) lead c1 c2 = cardGE (Direction dir) lead c1 c2
cardGE (Direction dir) lead (Card s1 r1) (Card s2 r2) =
    (s1 == lead && s2 /= lead) || (s1 == lead || s2 /= lead) && case dir of
        TopDown  -> r1 >= r2
        BottomUp -> r1 <= r2

highestCard :: Variant -> Suit -> NonEmpty Card -> Card
highestCard var lead = maximumBy (compareCard var lead)

playableCards :: Variant -> Suit -> [Card] -> Cards -> Cards
playableCards _   _    []       hand = hand
playableCards var lead (c : cs) hand = case var of
    Trump trump -> if lead == trump
        then if Set.null $ Set.filter (not . isPuur trump) $ follows trump
            then hand
            else follows trump
        else
            let followAndTrump = Set.union (follows trump) followerOrAll
            in  if suit highest == trump
                    then Set.filter
                        (\x -> suit x /= trump || cardGE var lead x highest)
                        followAndTrump
                    else followAndTrump
    _ -> followerOrAll
  where
    follows s = Set.filter ((== s) . suit) hand
    highest       = highestCard var lead (c :| cs)
    followerOrAll = if Set.null $ follows lead then hand else follows lead

{-# LANGUAGE OverloadedStrings #-}

module Jasskell.Card.Internal where

import           Data.Aeson
import           Data.Foldable                  ( maximumBy )
import           Data.List                      ( sortOn )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Data.Vector.Sized             as Vector
import           Data.Vector.Sized              ( Vector )
import           Jasskell.Card.Suit
import           Jasskell.Card.Rank
import           Jasskell.Variant
import           System.Random
import           Text.Read                      ( readPrec )
import           GHC.TypeLits

data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Ord)

instance Show Card where
    show c = show (suit c) ++ " " ++ show (rank c)

instance Read Card where
    readPrec = Card <$> readPrec <*> readPrec

type Cards = Set Card

puur :: Suit -> Card
puur = flip Card Under

isPuur :: Suit -> Card -> Bool
isPuur t = (== puur t)

nell :: Suit -> Card
nell = flip Card Nine

isNell :: Suit -> Card -> Bool
isNell t = (== nell t)

allCards :: [Card]
allCards =
    [ Card { suit = s, rank = r } | s <- [Bells .. Leaves], r <- [Six .. Ace] ]

value :: Variant -> Card -> Int
value (Trump trump) c | isPuur trump c = 20
                      | isNell trump c = 14
value (Direction _) c | rank c == Eight = 8
value (Slalom _) c | rank c == Eight    = 8
value _ c                               = case rank c of
    Ace   -> 11
    King  -> 4
    Over  -> 3
    Under -> 2
    Ten   -> 10
    _     -> 0


compareCard :: Variant -> Suit -> Card -> Card -> Ordering
compareCard var lead c1 c2 | c1 `gt` c2 = GT
                           | c2 `gt` c1 = LT
                           | otherwise  = EQ
    where gt = cardGT var lead

cardGT :: Variant -> Suit -> Card -> Card -> Bool
cardGT var lead c1@(Card s1 r1) c2@(Card s2 r2) = c1 /= c2 && case var of
    Trump trump -> if s1 == trump && s2 == trump
        then
            r1
            == Under
            || (r2 /= Under && (r1 == Nine || r2 /= Nine && r1 > r2))
        else
            s1
            == trump
            || (s2 /= trump && cardGT (Direction TopDown) lead c1 c2)
    Slalom dir -> cardGT (Direction dir) lead c1 c2
    Direction dir ->
        s1
            == lead
            && (s2 /= lead || case dir of
                   TopDown  -> r1 > r2
                   BottomUp -> r1 < r2
               )


highestCard :: Variant -> Suit -> NonEmpty Card -> Card
highestCard var lead = maximumBy (compareCard var lead)

playableCards :: Variant -> [Card] -> Cards -> Cards
playableCards _   []       hand = hand
playableCards var (c : cs) hand = case var of
    Trump trump
        | lead == trump
        -> if Set.null $ Set.delete (puur trump) $ follows trump
            then hand
            else follows trump
        | suit highest == trump
        -> let
               undertrump =
                   Set.filter (cardGT var lead highest) $ follows trump
               s = Set.difference (Set.union followerOrAll (follows trump))
                                  undertrump
           in
               if Set.null s then hand else s
        | otherwise
        -> Set.union followerOrAll (follows trump)
    _ -> followerOrAll
  where
    lead = suit c
    follows s = Set.filter ((== s) . suit) hand
    highest       = highestCard var lead (c :| cs)
    followerOrAll = if Set.null $ follows lead then hand else follows lead

instance ToJSON Card where
    toJSON c = object ["suit" .= suit c, "rank" .= rank c]

instance FromJSON Card where
    parseJSON = withObject "card" $ \o -> Card <$> o .: "suit" <*> o .: "rank"

dealCards :: (RandomGen g, KnownNat n) => g -> (Vector n Cards, g)
dealCards g = (v, g'')
  where
    v = Vector.generate
        (\f ->
            let i = fromIntegral f in Set.fromList $ take n $ drop (n * i) cs
        )
    l         = Vector.length v
    n         = 36 `div` l
    (g', g'') = split g
    cs =
        map snd
            $ sortOn fst
            $ zip (randoms g' :: [Int])
            $ (case 36 `mod` l of
                  0 -> id
                  1 -> tail
                  _ -> error ("invalid number of players: " ++ show n)
              )
                  allCards

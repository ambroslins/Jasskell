module Jasskell.Card
  ( Card (..),
    Suit (..),
    Rank (..),
    Cards,
    deck,
    value,
    compare,
    Reason (..),
    playable,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (maximumBy)
import Data.Set qualified as Set
import Jasskell.Card.Suit (Suit (..))
import Jasskell.Variant (Direction (..), Variant (..))
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

data Reason
  = NotInHand
  | FollowTrump Suit
  | FollowLead Suit
  | Undertrump Card
  deriving (Eq, Show)

playable ::
  forall m. MonadError Reason m => Variant -> [Card] -> Cards -> Card -> m Cards
playable variant table hand card = Set.alterF check card hand
  where
    check :: Bool -> m Bool
    check hasCard = do
      unless hasCard $ throwError NotInHand
      case table of
        [] -> pure ()
        (c : cs) -> do
          let lead = suit c
              followers = Set.filter ((== lead) . suit) hand
              comp = compare variant lead
          case variant of
            Trump trump
              | lead == trump ->
                unless
                  (suit card == trump || Set.null (Set.delete puur trumps))
                  $ throwError (FollowTrump trump)
              | suit highest == trump ->
                if suit card == trump
                  then
                    unless
                      ( comp card highest == GT
                          || (Set.null highers && hand == trumps)
                      )
                      $ throwError (Undertrump highest)
                  else
                    unless
                      (suit card == lead || Set.null followers)
                      $ throwError (FollowLead lead)
              | otherwise ->
                unless
                  (suit card `elem` [lead, trump] || Set.null followers)
                  $ throwError (FollowLead lead)
              where
                trumps = Set.filter ((== trump) . suit) hand
                highest = maximumBy comp (c :| cs)
                highers = Set.filter (\x -> comp x highest == GT) hand
                puur = Card trump Under
            _ ->
              unless
                (suit card == lead || Set.null followers)
                $ throwError (FollowLead lead)
      pure False

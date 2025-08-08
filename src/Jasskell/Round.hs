module Jasskell.Round
  ( Round,
    close,
    variant,
    tricks,
    leader,
    shoved,
  )
where

import Control.Exception (assert)
import Data.Foldable (toList)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Vector4 (Index4 (..), Vector4 (..))
import Data.Vector4 qualified as Vector4
import Jasskell.Card (Card, CardSet, Suit)
import Jasskell.Card qualified as Card
import Jasskell.Trick (Trick)
import Jasskell.Trick qualified as Trick
import Jasskell.Variant (Variant (..))
import Jasskell.Variant qualified as Variant
import System.Random (RandomGen)

data Round = Round
  { variant :: !Variant,
    tricks :: ![Trick],
    leader :: !Index4,
    shoved :: !Bool
  }
  deriving (Show)

close :: Index4 -> Bool -> Variant -> [Trick] -> Round
close leader shoved variant tricks =
  Round {leader, shoved, variant, tricks}

{-
data Round = Round
  { variant :: !(Maybe Variant),
    hands :: !(Vector4 CardSet),
    table :: !(Vector4 (Maybe Card)),
    tricks :: ![Trick],
    leader :: !Index4,
    shoved :: !Bool
  }
  deriving (Show)

new :: (RandomGen g) => Maybe Index4 -> g -> (Round, g)
new l gen =
  let (hands, gen') = Card.deal gen
      hasWeli = case Vector4.findIndex (Card.member Card.weli) hands of
        Just i -> i
        Nothing -> error "Jasskell.Round.new: no weli card"
   in ( Round
          { variant = Nothing,
            hands,
            table = Vector4.replicate Nothing,
            tricks = [],
            leader = fromMaybe hasWeli l,
            shoved = False
          },
        gen'
      )

trickLeader :: Round -> Index4
trickLeader Round {variant, leader, shoved, tricks} = case variant of
  Nothing
    | shoved -> leader + 2
    | otherwise -> leader
  Just _ -> case tricks of
    [] -> leader
    t : _ -> Trick.winner t

currentPlayer :: Round -> Index4
currentPlayer r =
  foldl'
    (\i mc -> if isJust mc then succ i else i)
    (trickLeader r)
    (table r)

declareVariant :: Variant -> Round -> Maybe Round
declareVariant v r = case variant r of
  Nothing -> Just r {variant = Just v}
  Just _ -> Nothing

data ShoveError
  = VariantAlreadyDefined
  | AlreadyShoved
  deriving (Eq, Show)

shove :: Round -> Either ShoveError Round
shove r = case variant r of
  Just _ -> Left VariantAlreadyDefined
  Nothing
    | shoved r -> Left AlreadyShoved
    | otherwise -> pure r {shoved = True}

data UnplayableCardReason
  = NoVariant
  | NotInHand
  | FollowLead Suit
  | FollowTrump Suit
  | Undertrump Card
  deriving (Eq, Show)

isCardPlayable :: Round -> Card -> Either UnplayableCardReason ()
isCardPlayable r card
  | not (Card.member card hand) = Left NotInHand
  | otherwise = case variant r of
      Nothing -> Left NoVariant
      Just v -> case catMaybes $ toList $ Vector4.rotate (leader r) (table r) of
        [] -> pure ()
        c : cs -> case v of
          Trump trump
            | trump == lead ->
                let puur = Card.make trump Card.Under
                    trumps = Card.filter (isOfSuit trump) hand
                 in if Card.suit c == trump
                      || Card.null (Card.delete puur trumps)
                      then Right ()
                      else Left $ FollowTrump trump
            | Card.suit highest == trump && Card.suit c == trump ->
                if Card.compare lead v c highest == GT
                  then Right ()
                  else Left $ Undertrump highest
            | Card.suit c == trump -> Right ()
          _
            | Card.suit c == lead || Card.null followers -> Right ()
            | otherwise -> Left $ FollowLead lead
          where
            lead = Card.suit c
            followers = Card.filter (isOfSuit lead) hand
            highest = foldl' (Card.max lead v) c cs
  where
    current = currentPlayer r
    hand = Vector4.index current (hands r)
    isOfSuit s c = Card.suit c == s

playCard :: Card -> Round -> Either UnplayableCardReason Round
playCard card r = case variant r of
  Nothing -> Left NoVariant
  Just v -> do
    isCardPlayable r card
    let nextTable =
          Vector4.modify
            current
            (\mc -> assert (isNothing mc) $ Just card)
            (table r)
    pure $ case sequence nextTable of
      Nothing -> r {table = nextTable}
      Just t ->
        r
          { table = Vector4.replicate Nothing,
            tricks = Trick.close v (trickLeader r) t : tricks r,
            variant = Just $ Variant.next v
          }
  where
    current = currentPlayer r

completed :: Round -> Bool
completed = all Card.null . hands
-}

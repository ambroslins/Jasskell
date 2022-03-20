module View
  ( View,
    Declaring,
    Playing,
    PlayableCard (card),
    UnplayableError (..),
    makeDeclaring,
    makePlaying,
    checkCard,
    hand,
    eldest,
    leader,
    current,
    variant,
    trick,
  )
where

import Card (Card (Card), Cards, Suit)
import Card qualified
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeNats (KnownNat)
import List qualified
import Map qualified
import Set qualified
import Variant (Variant (Trump))

type View v n = Finite n -> v n

data Declaring n = Declaring
  { hand :: Cards,
    eldest :: Finite n
  }
  deriving (Show)

makeDeclaring :: KnownNat n => Finite n -> Vector n Cards -> View Declaring n
makeDeclaring e hands player =
  Declaring
    { hand = Vector.index hands player,
      eldest = e - player
    }

data Playing n = Playing
  { hand :: Cards,
    leader :: Finite n,
    variant :: Variant,
    trick :: [Card]
  }
  deriving (Show)

makePlaying :: KnownNat n => Finite n -> Variant -> Vector n Cards -> [Card] -> View Playing n
makePlaying absoluteLeader var hands cards player =
  Playing
    { hand = Vector.index hands player,
      leader = absoluteLeader - player,
      variant = var,
      trick = cards
    }

newtype PlayableCard = PlayableCard {card :: Card}
  deriving newtype (Eq, Ord, Show)

data UnplayableError
  = NotInHand
  | NotYourTurn
  | FollowTrump Suit
  | FollowLead Suit
  | Undertrump Card
  deriving (Eq, Show)

checkCard :: KnownNat n => Playing n -> Card -> Either UnplayableError PlayableCard
checkCard view card
  | current view /= 0 = Left NotYourTurn
  | Set.notMember card view.hand = Left NotInHand
  | otherwise = case view.trick of
      [] -> Right $ PlayableCard card
      (leadCard : cs) -> case view.variant of
        Trump trump
          | leadSuit == trump ->
              check (FollowTrump trump) $
                cardSuit == trump || Set.null (Set.delete puur trumps)
          | Card.suit highest == trump ->
              if cardSuit == trump
                then
                  check (Undertrump highest) $
                    comp card highest == GT
                      || (Set.null highers && Set.isSubsetOf view.hand trumps)
                else
                  check (FollowLead leadSuit) $
                    cardSuit == leadSuit || Set.null followers
          | otherwise ->
              check (FollowLead leadSuit) $
                cardSuit `List.elem` [leadSuit, trump] || Set.null followers
          where
            trumps = Set.filter ((== trump) . Card.suit) view.hand
            highest = maximumBy comp (leadCard :| cs)
            highers = Set.filter (\x -> comp x highest == GT) view.hand
            puur = Card trump Card.Under
        _ ->
          check (FollowLead leadSuit) $
            cardSuit == leadSuit || Set.null followers
        where
          cardSuit = Card.suit card
          leadSuit = Card.suit leadCard
          followers = Set.filter ((== leadSuit) . Card.suit) view.hand
          comp = Card.compare view.variant leadSuit
          check r p = if p then Right $ PlayableCard card else Left r

class HasHand v where
  hand :: v -> Cards

instance HasHand (Declaring n) where
  hand view = view.hand

instance HasHand (Playing n) where
  hand view = view.hand

eldest :: Declaring n -> Finite n
eldest view = view.eldest

leader :: Playing n -> Finite n
leader view = view.leader

current :: KnownNat n => Playing n -> Finite n
current view = view.leader + fromIntegral (length view.trick)

variant :: Playing n -> Variant
variant view = view.variant

trick :: KnownNat n => Playing n -> Map (Finite n) Card
trick view = Map.fromList $ List.zip [view.leader ..] view.trick

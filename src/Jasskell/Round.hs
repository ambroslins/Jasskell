module Jasskell.Round
  ( Round,
    Seat,
    start,
    chooser,
    leader,
    currentPlayer,
    variant,
    shoved,
    handOf,
    playedCards,
    ShoveError (..),
    shove,
    ChooseError (..),
    choose,
    PlayError (..),
    playCard,
    close,
    Hand (cards),
    CardStatus (..),
    Violation (..),
    foldHand,
  )
where

import Control.Monad (guard, unless, when)
import Data.Fin (Fin)
import Data.Fin qualified as Fin
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust)
import Data.Vec (Vec)
import Data.Vec qualified as Vec
import GHC.TypeLits (KnownNat)
import Jasskell.Card (Card, Rank (Under), Suit)
import Jasskell.Card qualified as Card
import Jasskell.Card.Seq (CardSeq)
import Jasskell.Card.Seq qualified as CardSeq
import Jasskell.Card.Set (CardSet)
import Jasskell.Card.Set qualified as CardSet
import Jasskell.Variant (Variant (..))
import Jasskell.Variant qualified as Variant
import Prelude hiding (round)

type Seat = Fin

data Round n = Round
  { chooser :: Seat n,
    hands :: Vec n CardSet,
    variant :: Maybe Variant,
    played :: CardSeq,
    tricks :: [Trick n],
    points :: Vec n Int,
    shoved :: Bool
  }
  deriving (Show)

data Trick n = Trick
  { cards :: CardSeq,
    leader :: Seat n,
    winner :: Seat n,
    points :: Int
  }
  deriving (Show)

data Result n = Result
  { trickPoints :: Vec n Int,
    stoeck :: Maybe (Seat n)
  }

start :: (KnownNat n) => Seat n -> Vec n CardSet -> Round n
start c hs =
  Round
    { chooser = c,
      hands = hs,
      variant = Nothing,
      played = CardSeq.empty,
      tricks = [],
      points = Vec.replicate 0,
      shoved = False
    }

chooser :: Round n -> Seat n
chooser round = round.chooser

leader :: Round n -> Seat n
leader round = case round.tricks of
  [] -> round.chooser
  t : _ -> t.winner

currentPlayer :: (KnownNat n) => Round n -> Seat n
currentPlayer round = case round.variant of
  Nothing
    | round.shoved -> partner round.chooser
    | otherwise -> round.chooser
  Just _ -> leader round + Fin.unsafe (CardSeq.length round.played)

variant :: Round n -> Maybe Variant
variant round = round.variant

shoved :: Round n -> Bool
shoved round = round.shoved

handOf :: (KnownNat n) => Seat n -> Round n -> Hand
handOf seat round = Hand {cards, state}
  where
    cards = Vec.index round.hands seat
    state = case round.variant of
      Nothing -> Choosing
      Just v
        | seat == currentPlayer round ->
            Active $ restrictionsFor v (CardSeq.toList round.played) cards
        | otherwise -> Passive

playedCards :: (KnownNat n) => Round n -> Vec n (Maybe Card)
playedCards round = Vec.create Nothing $ \write -> do
  CardSeq.foldM_
    (\i c -> write i (Just c) $> i + 1)
    (leader round)
    round.played

data ShoveError
  = CannotShoveNow
  | AlreadyShoved
  deriving (Eq, Show)

shove :: Round n -> Either ShoveError (Round n)
shove round
  | isJust round.variant = Left CannotShoveNow
  | round.shoved = Left AlreadyShoved
  | otherwise = Right round {shoved = True}

data ChooseError = CannotChooseNow
  deriving (Eq, Show)

choose :: Variant -> Round n -> Either ChooseError (Round n)
choose v round
  | isJust round.variant = Left CannotChooseNow
  | otherwise = Right round {variant = Just v}

data PlayError
  = CannotPlayNow
  | CardNotInHand
  | ForbiddenCard Violation
  deriving (Eq, Show)

playCard :: (KnownNat n) => Card -> Round n -> Either PlayError (Round n)
playCard card round = do
  v <- maybe (Left CannotPlayNow) pure round.variant
  let current = currentPlayer round
      hand = Vec.index round.hands current
  unless (card `CardSet.member` hand) $ Left CardNotInHand
  let restrictions = restrictionsFor v (CardSeq.toList round.played) hand
  for_ restrictions $ \r ->
    when (card `CardSet.member` r.set) $ Left $! ForbiddenCard r.violation
  pure $!
    fromMaybe <*> closeTrick $
      round
        { played = CardSeq.push card round.played,
          hands = Vec.set current (CardSet.delete card hand) round.hands
        }

closeTrick :: forall n. (KnownNat n) => Round n -> Maybe (Round n)
closeTrick round = do
  let cards = round.played
  guard $ CardSeq.length cards == Fin.nat n
  v <- round.variant
  let l = leader round
      lead = Card.suit $ CardSeq.unsafeIndex cards (Fin.toInt l)
      trick =
        Trick
          { leader = l,
            cards,
            winner = l + Fin.unsafe (CardSeq.maxIndexBy (Card.compare lead v) cards),
            points = CardSeq.foldl' (\p c -> p + Card.points v c) 0 cards
          }
  pure $!
    round
      { played = CardSeq.empty,
        tricks = trick : round.tricks,
        variant = Just $ Variant.next v,
        points = Vec.modify trick.winner (+ trick.points) round.points
      }

close :: (KnownNat n) => Round n -> Maybe (Result n)
close round = do
  guard $ Vec.all CardSet.null round.hands
  pure
    Result
      { trickPoints = round.points,
        stoeck = Nothing -- TODO
      }

partner :: (KnownNat n) => Seat n -> Seat n
partner = (+ 2)

data Hand = Hand
  { cards :: CardSet,
    state :: State
  }
  deriving (Eq, Show)

data CardStatus
  = Playable
  | VariantNotChosen
  | NotYourTurn
  | Forbidden Violation
  deriving (Eq, Show)

data Violation
  = DoesNotFollow Suit
  | WouldUndertrump Card
  deriving (Eq, Show)

data State
  = Choosing
  | Passive
  | Active [Restriction]
  deriving (Eq, Show)

data Restriction = Restriction
  { set :: CardSet,
    violation :: Violation
  }
  deriving (Eq, Show)

statusOf :: State -> Card -> CardStatus
statusOf state = case state of
  Choosing -> const VariantNotChosen
  Passive -> const NotYourTurn
  Active restrictions -> \card ->
    foldr
      (\r s -> if card `CardSet.member` r.set then Forbidden r.violation else s)
      Playable
      restrictions
{-# INLINE statusOf #-}

foldHand :: (Monoid m) => Hand -> (Card -> CardStatus -> m) -> m
foldHand hand f = CardSet.foldr go mempty hand.cards
  where
    status = statusOf hand.state
    go c m = f c (status c) <> m
{-# INLINE foldHand #-}

restrictionsFor :: Variant -> [Card] -> CardSet -> [Restriction]
restrictionsFor v trick cards = case trick of
  [] -> []
  c : cs ->
    restrict doesNotFollow (DoesNotFollow lead) $
      restrict wouldUndertrump (WouldUndertrump highest) []
    where
      lead = Card.suit c
      follower = CardSet.filterSuit lead cards
      obligation = case v of
        Trump trump -> CardSet.delete (Card.make trump Under) follower
        _ -> follower
      trumps = case v of
        Trump trump -> CardSet.filterSuit trump cards
        _ -> CardSet.empty
      doesNotFollow
        | CardSet.null obligation = CardSet.empty
        | otherwise =
            cards `CardSet.difference` follower `CardSet.difference` trumps
      highest = foldl' (Card.max lead v) c cs
      wouldUndertrump = case v of
        Trump trump
          | lead /= trump && cards /= trumps && Card.suit highest == trump ->
              let comp = Card.compare lead v
               in CardSet.filter (\card -> comp card highest == LT) trumps
        _ -> CardSet.empty
      restrict !set !violation !rest
        | CardSet.null set = rest
        | otherwise = Restriction {set, violation} : rest

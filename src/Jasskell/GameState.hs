module Jasskell.GameState
  ( GameState,
    new,
    currentPlayer,
    trickLeader,
    DeclareError (..),
    declareVariant,
    ShoveError (..),
    shove,
    playCard,
    closeTrick,
    closeRound,
    GameView (..),
    HandCard (..),
    CardStatus (..),
    IllegalCard (..),
    viewFor,
  )
where

import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as VU
import Data.Vector4 (Index4 (..), Vector4)
import Data.Vector4 qualified as Vector4
import Jasskell.Card (Card, Rank (..), Suit)
import Jasskell.Card qualified as Card
import Jasskell.Card.Seq (CardSeq)
import Jasskell.Card.Seq qualified as CardSeq
import Jasskell.Card.Set (CardSet)
import Jasskell.Card.Set qualified as CardSet
import Jasskell.Variant (Variant (..))
import Jasskell.Variant qualified as Variant
import System.Random qualified as Random
import Prelude hiding (round)

data GameState = GameState
  { randomGen :: Random.StdGen,
    rounds :: [Round],
    variant :: Maybe Variant,
    hands :: Vector4 CardSet,
    playedCards :: CardSeq,
    tricks :: [Trick],
    leader :: Index4,
    shoved :: Bool
  }
  deriving (Show)

data Round = Round
  { variant :: Variant,
    tricks :: [Trick],
    leader :: Index4,
    shoved :: Bool
  }
  deriving (Show)

data Trick = Trick
  { cards :: CardSeq,
    leader :: Index4,
    winner :: Index4,
    points :: Int
  }
  deriving (Show)

new :: Random.StdGen -> GameState
new gen =
  GameState
    { randomGen = gen',
      rounds = [],
      variant = Nothing,
      hands,
      playedCards = CardSeq.empty,
      tricks = [],
      leader =
        fromMaybe
          (error "Jasskell.GameState.new: no weli card")
          (Vector4.findIndex (CardSet.member Card.weli) hands),
      shoved = False
    }
  where
    (hands, gen') = deal gen

deal :: Random.StdGen -> (Vector4 CardSet, Random.StdGen)
deal = first split . CardSet.shuffle
  where
    split v =
      Vector4.generate $ \i ->
        VU.foldl' (flip CardSet.insert) CardSet.empty $
          VU.slice (fromEnum i * 9) 9 v

trickLeader :: GameState -> Index4
trickLeader gs = case gs.tricks of
  [] -> gs.leader
  t : _ -> t.winner

currentPlayer :: GameState -> Index4
currentPlayer game = case game.variant of
  Nothing
    | game.shoved -> game.leader + 2
    | otherwise -> game.leader
  Just _ -> case closeTrick game of
    Just gs -> trickLeader gs
    Nothing -> trickLeader game + toEnum (CardSeq.length game.playedCards)

data DeclareError = VariantAlreadyDeclared
  deriving (Eq, Show)

declareVariant :: Variant -> GameState -> Either DeclareError GameState
declareVariant v gs = case gs.variant of
  Just _ -> Left VariantAlreadyDeclared
  -- We need to update some other field to make the record update unambiguous.
  Nothing -> Right gs {variant = Just v, randomGen = gs.randomGen}

data ShoveError
  = AlreadyShoved
  | VariantAlreadyDefined
  deriving (Eq, Show)

shove :: GameState -> Either ShoveError GameState
shove gs = case gs.variant of
  Just _ -> Left VariantAlreadyDefined
  Nothing
    | gs.shoved -> Left AlreadyShoved
    -- We need to update some other field to make the record update unambiguous.
    | otherwise -> Right gs {shoved = True, randomGen = gs.randomGen}

data IllegalCard
  = VariantNotChosen
  | NotYourTurn Index4
  | NotInHand
  | DoesNotFollow Suit
  | WouldUndertrump Card
  deriving (Eq, Show)

data CardStatus = Playable | Illegal IllegalCard
  deriving (Eq, Show)

-- | The 'CardStatus' for the current player.
--
-- The card sets are computed once per 'GameState'. Bind the partial
-- application to share them across cards:
--
-- > let status = currentCardStatus gameState
-- >  in map status (CardSet.toList hand)
--
-- Callers holding another seat must check turn order themselves; this
-- function assumes the caller is 'currentPlayer'.
currentCardStatus :: GameState -> Card -> CardStatus
currentCardStatus gameState = \card ->
  if card `CardSet.member` hand then status card else Illegal NotInHand
  where
    !gs = fromMaybe gameState $ closeTrick gameState
    !hand = Vector4.index (currentPlayer gs) gs.hands
    !status = case gs.variant of
      Nothing -> const $ Illegal VariantNotChosen
      Just variant -> case CardSeq.toList gs.playedCards of
        [] -> const Playable
        c : cs ->
          \case
            card
              | card `CardSet.member` doesNotFollow -> Illegal $ DoesNotFollow lead
              | card `CardSet.member` wouldUndertrump -> Illegal $ WouldUndertrump highest
              | otherwise -> Playable
          where
            !lead = Card.suit c
            !follower = CardSet.filterSuit lead hand
            !obligation = case variant of
              Trump trump -> CardSet.delete (Card.make trump Under) follower
              _ -> follower
            !trumps = case variant of
              Trump trump -> CardSet.filterSuit trump hand
              _ -> CardSet.empty
            !doesNotFollow
              | CardSet.null obligation = CardSet.empty
              | otherwise = hand `CardSet.difference` follower `CardSet.difference` trumps
            !highest = foldl' (Card.max lead variant) c cs
            !wouldUndertrump = case variant of
              Trump trump
                | lead /= trump && hand /= trumps && Card.suit highest == trump ->
                    let comp = Card.compare lead variant
                     in CardSet.filter (\card -> comp card highest == LT) trumps
              _ -> CardSet.empty

playCard :: Card -> GameState -> Either IllegalCard GameState
playCard card gameState = case currentCardStatus gs card of
  Illegal e -> Left e
  Playable ->
    pure $
      gs
        { playedCards = CardSeq.push card gs.playedCards,
          hands = Vector4.modify current (CardSet.delete card) gs.hands
        }
  where
    gs = fromMaybe gameState (closeTrick gameState)
    current = currentPlayer gs

closeTrick :: GameState -> Maybe GameState
closeTrick gs = do
  let cards = gs.playedCards
  guard $ CardSeq.length cards == 4
  v <- gs.variant
  let leader = trickLeader gs
      lead = Card.suit $ CardSeq.unsafeIndex cards (fromEnum leader)
      trick =
        Trick
          { leader,
            cards,
            winner =
              leader
                + fromIntegral (CardSeq.maxIndexBy (Card.compare lead v) cards),
            points = CardSeq.foldl' (\p c -> p + Card.points v c) 0 cards
          }
  pure
    gs
      { playedCards = CardSeq.empty,
        tricks = trick : gs.tricks,
        variant = Just $ Variant.next v
      }

closeRound :: GameState -> Maybe GameState
closeRound game = do
  let gs = fromMaybe game (closeTrick game)
  v <- gs.variant
  guard $ all CardSet.null gs.hands
  let round =
        Round
          { leader = gs.leader,
            shoved = gs.shoved,
            variant = v,
            tricks = reverse gs.tricks
          }
      (newHands, gen) = deal gs.randomGen
  pure
    GameState
      { randomGen = gen,
        rounds = round : gs.rounds,
        variant = Nothing,
        hands = newHands,
        playedCards = CardSeq.empty,
        tricks = [],
        leader = gs.leader + 1,
        shoved = False
      }

data HandCard = HandCard {card :: Card, status :: CardStatus}
  deriving (Show)

data GameView = GameView
  { variant :: Maybe Variant,
    hand :: [HandCard],
    playedCards :: CardSeq,
    leader :: Index4,
    trickLeader :: Index4,
    currentPlayer :: Index4,
    shoved :: Bool
  }
  deriving (Show)

viewFor :: Index4 -> GameState -> GameView
viewFor i gameState =
  GameView
    { variant = gameState.variant,
      hand = map (\card -> HandCard {card, status = statusOf card}) handCards,
      playedCards = gameState.playedCards,
      leader = gameState.leader - i,
      trickLeader = trickLeader gameState - i,
      currentPlayer = current - i,
      shoved = gameState.shoved
    }
  where
    handCards = CardSet.toList $ Vector4.index i gameState.hands
    current = currentPlayer gameState
    statusOf =
      if current == i
        then currentCardStatus gameState
        else const $ Illegal $ NotYourTurn $ current - i

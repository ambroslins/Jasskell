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
import Data.Fin (Fin)
import Data.Fin qualified as Fin
import Data.Maybe (fromMaybe)
import Data.Vec (Vec)
import Data.Vec qualified as Vec
import Data.Vector.Unboxed qualified as VU
import GHC.TypeLits (KnownNat)
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

data GameState n = GameState
  { randomGen :: Random.StdGen,
    rounds :: [Round n],
    variant :: Maybe Variant,
    hands :: Vec n CardSet,
    playedCards :: CardSeq,
    tricks :: [Trick n],
    leader :: Fin n,
    shoved :: Bool
  }
  deriving (Show)

data Round n = Round
  { variant :: Variant,
    tricks :: [Trick n],
    leader :: Fin n,
    shoved :: Bool
  }
  deriving (Show)

data Trick n = Trick
  { cards :: CardSeq,
    leader :: Fin n,
    winner :: Fin n,
    points :: Int
  }
  deriving (Show)

new :: (KnownNat n) => Random.StdGen -> GameState n
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
          (Vec.findIndex (CardSet.member Card.weli) hands),
      shoved = False
    }
  where
    (hands, gen') = deal gen

deal :: (KnownNat n) => Random.StdGen -> (Vec n CardSet, Random.StdGen)
deal = first split . CardSet.shuffle
  where
    split v =
      Vec.generate $ \i ->
        VU.foldl' (flip CardSet.insert) CardSet.empty $
          VU.slice (Fin.toInt i * 9) 9 v

trickLeader :: GameState n -> Fin n
trickLeader gs = case gs.tricks of
  [] -> gs.leader
  t : _ -> t.winner

currentPlayer :: (KnownNat n) => GameState n -> Fin n
currentPlayer game = case game.variant of
  Nothing
    | game.shoved -> game.leader + 2
    | otherwise -> game.leader
  Just _ -> case closeTrick game of
    Just gs -> trickLeader gs
    Nothing -> trickLeader game + Fin.unsafe (CardSeq.length game.playedCards)

data DeclareError = VariantAlreadyDeclared
  deriving (Eq, Show)

declareVariant :: Variant -> GameState n -> Either DeclareError (GameState n)
declareVariant v gs = case gs.variant of
  Just _ -> Left VariantAlreadyDeclared
  -- We need to update some other field to make the record update unambiguous.
  Nothing -> Right gs {variant = Just v, randomGen = gs.randomGen}

data ShoveError
  = AlreadyShoved
  | VariantAlreadyDefined
  deriving (Eq, Show)

shove :: GameState n -> Either ShoveError (GameState n)
shove gs = case gs.variant of
  Just _ -> Left VariantAlreadyDefined
  Nothing
    | gs.shoved -> Left AlreadyShoved
    -- We need to update some other field to make the record update unambiguous.
    | otherwise -> Right gs {shoved = True, randomGen = gs.randomGen}

data IllegalCard n
  = VariantNotChosen
  | NotYourTurn (Fin n)
  | NotInHand
  | DoesNotFollow Suit
  | WouldUndertrump Card
  deriving (Eq, Show)

data CardStatus n = Playable | Illegal (IllegalCard n)
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
currentCardStatus :: (KnownNat n) => GameState n -> Card -> CardStatus n
currentCardStatus gameState = \card ->
  if card `CardSet.member` hand then status card else Illegal NotInHand
  where
    !gs = fromMaybe gameState $ closeTrick gameState
    !hand = Vec.index gs.hands (currentPlayer gs)
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

playCard :: (KnownNat n) => Card -> GameState n -> Either (IllegalCard n) (GameState n)
playCard card gameState = case currentCardStatus gs card of
  Illegal e -> Left e
  Playable ->
    pure $
      gs
        { playedCards = CardSeq.push card gs.playedCards,
          hands = Vec.modify current (CardSet.delete card) gs.hands
        }
  where
    gs = fromMaybe gameState (closeTrick gameState)
    current = currentPlayer gs

closeTrick :: (KnownNat n) => GameState n -> Maybe (GameState n)
closeTrick gs = do
  let cards = gs.playedCards
  guard $ CardSeq.length cards == 4
  v <- gs.variant
  let leader = trickLeader gs
      lead = Card.suit $ CardSeq.unsafeIndex cards (Fin.toInt leader)
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

closeRound :: GameState 4 -> Maybe (GameState 4)
closeRound game = do
  let gs = fromMaybe game (closeTrick game)
  v <- gs.variant
  guard $ Vec.all CardSet.null gs.hands
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

data HandCard n = HandCard {card :: Card, status :: CardStatus n}
  deriving (Show)

data GameView n = GameView
  { variant :: Maybe Variant,
    hand :: [HandCard n],
    playedCards :: CardSeq,
    leader :: Fin n,
    trickLeader :: Fin n,
    currentPlayer :: Fin n,
    shoved :: Bool
  }
  deriving (Show)

viewFor :: (KnownNat n) => Fin n -> GameState n -> GameView n
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
    handCards = CardSet.toList $ Vec.index gameState.hands i
    current = currentPlayer gameState
    statusOf =
      if current == i
        then currentCardStatus gameState
        else const $ Illegal $ NotYourTurn $ current - i

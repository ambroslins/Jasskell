module Jasskell.GameState
  ( GameState,
    new,
    currentPlayer,
    trickLeader,
    DeclareError (..),
    declareVariant,
    ShoveError (..),
    shove,
    UnplayableCardReason (..),
    isCardPlayable,
    playCard,
    closeTrick,
    closeRound,
    view,
  )
where

import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Vector4 (Index4 (..), Vector4)
import Data.Vector4 qualified as Vector4
import Jasskell.Card (Card, CardSet, Suit)
import Jasskell.Card qualified as Card
import Jasskell.GameView (GameView (GameView))
import Jasskell.GameView qualified as GameView
import Jasskell.Variant (Variant (..))
import Jasskell.Variant qualified as Variant
import System.Random qualified as Random
import Prelude hiding (round)

data GameState = GameState
  { randomGen :: !Random.StdGen,
    rounds :: ![Round],
    variant :: !(Maybe Variant),
    hands :: !(Vector4 CardSet),
    playedCards :: !(Vector4 (Maybe Card)),
    tricks :: ![Trick],
    leader :: !Index4,
    shoved :: !Bool
  }
  deriving (Show)

data Round = Round
  { variant :: !Variant,
    tricks :: ![Trick],
    leader :: !Index4,
    shoved :: !Bool
  }
  deriving (Show)

data Trick = Trick
  { cards :: !(Vector4 Card),
    leader :: !Index4,
    winner :: !Index4,
    points :: !Int
  }
  deriving (Show)

new :: Random.StdGen -> GameState
new gen =
  GameState
    { randomGen = gen',
      rounds = [],
      variant = Nothing,
      hands,
      playedCards = Vector4.replicate Nothing,
      tricks = [],
      leader =
        fromMaybe
          (error "Jasskell.GameState.new: no weli card")
          (Vector4.findIndex (Card.member Card.weli) hands),
      shoved = False
    }
  where
    (hands, gen') = Card.deal gen

trickLeader :: GameState -> Index4
trickLeader gs = case gs.variant of
  Nothing
    | gs.shoved -> gs.leader + 2
    | otherwise -> gs.leader
  Just _ -> case gs.tricks of
    [] -> gs.leader
    t : _ -> t.winner

currentPlayer :: GameState -> Index4
currentPlayer game = case closeTrick game of
  Just gs -> trickLeader gs
  Nothing ->
    foldl'
      (\i mc -> if isJust mc then i + 1 else i)
      (trickLeader game)
      game.playedCards

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

data UnplayableCardReason
  = NoVariant
  | NotInHand
  | FollowLead Suit
  | FollowTrump Suit
  | Undertrump Card
  deriving (Eq, Show)

isCardPlayable :: GameState -> Card -> Either UnplayableCardReason ()
isCardPlayable game card
  | not (Card.member card hand) = Left NotInHand
  | Just _ <- closeTrick game = pure ()
  | otherwise = case gs.variant of
      Nothing -> Left NoVariant
      Just v -> case catMaybes $ toList $ Vector4.rotate gs.leader gs.playedCards of
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
    gs = fromMaybe game (closeTrick game)
    current = currentPlayer gs
    hand = Vector4.index current gs.hands
    isOfSuit s c = Card.suit c == s

playCard :: Card -> GameState -> Either UnplayableCardReason GameState
playCard card game = case gs.variant of
  Nothing -> Left NoVariant
  Just _ ->
    isCardPlayable gs card
      $> gs
        { playedCards = Vector4.set current (Just card) gs.playedCards,
          hands = Vector4.modify current (Card.delete card) gs.hands
        }
  where
    gs = fromMaybe game (closeTrick game)
    current = currentPlayer gs

closeTrick :: GameState -> Maybe GameState
closeTrick gs = do
  v <- gs.variant
  cs <- sequence gs.playedCards
  let leader = trickLeader gs
      leadSuit = Card.suit $ Vector4.index leader cs
      trick =
        Trick
          { leader,
            cards = cs,
            winner =
              leader
                + Vector4.maxIndexBy
                  (Card.compare leadSuit v)
                  (Vector4.rotate leader cs),
            points = foldl' (\p c -> p + Card.points v c) 0 cs
          }
  pure
    gs
      { playedCards = Vector4.replicate Nothing,
        tricks = trick : gs.tricks,
        variant = Just $ Variant.next v
      }

closeRound :: GameState -> Maybe GameState
closeRound game = do
  let gs = fromMaybe game (closeTrick game)
  v <- gs.variant
  guard $ all Card.null gs.hands
  let round =
        Round
          { leader = gs.leader,
            shoved = gs.shoved,
            variant = v,
            tricks = reverse gs.tricks
          }
      (newHands, gen) = Card.deal gs.randomGen
  pure
    GameState
      { randomGen = gen,
        rounds = round : gs.rounds,
        variant = Nothing,
        hands = newHands,
        playedCards = Vector4.replicate Nothing,
        tricks = [],
        leader = gs.leader + 1,
        shoved = False
      }

view :: Index4 -> GameState -> GameView
view player gs =
  GameView
    { variant = gs.variant,
      hand = Vector4.index player gs.hands,
      playedCards = Vector4.rotate player gs.playedCards,
      leader = gs.leader - player,
      currentPlayer = currentPlayer gs - player,
      shoved = gs.shoved
    }

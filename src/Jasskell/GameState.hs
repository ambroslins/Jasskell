module Jasskell.GameState
  ( GameState,
    rounds,
    variant,
    hands,
    playedCards,
    tricks,
    leader,
    shoved,
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
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Trick (Trick)
import Jasskell.Trick qualified as Trick
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
trickLeader g = case variant g of
  Nothing
    | shoved g -> leader g + 2
    | otherwise -> leader g
  Just _ -> case tricks g of
    [] -> leader g
    t : _ -> Trick.winner t

currentPlayer :: GameState -> Index4
currentPlayer game = case closeTrick game of
  Just g -> trickLeader g
  Nothing ->
    foldl'
      (\i mc -> if isJust mc then i + 1 else i)
      (trickLeader game)
      (playedCards game)

data DeclareError = VariantAlreadyDeclared
  deriving (Eq, Show)

declareVariant :: Variant -> GameState -> Either DeclareError GameState
declareVariant v gs = case variant gs of
  Just _ -> Left VariantAlreadyDeclared
  Nothing -> Right gs {variant = Just v}

data ShoveError
  = AlreadyShoved
  | VariantAlreadyDefined
  deriving (Eq, Show)

shove :: GameState -> Either ShoveError GameState
shove gs = case variant gs of
  Just _ -> Left VariantAlreadyDefined
  Nothing
    | shoved gs -> Left AlreadyShoved
    | otherwise -> Right gs {shoved = True}

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
  | otherwise = case variant g of
      Nothing -> Left NoVariant
      Just v -> case catMaybes $ toList $ Vector4.rotate (leader g) (playedCards g) of
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
    g = fromMaybe game (closeTrick game)
    current = currentPlayer g
    hand = Vector4.index current (hands g)
    isOfSuit s c = Card.suit c == s

playCard :: Card -> GameState -> Either UnplayableCardReason GameState
playCard card game = case variant g of
  Nothing -> Left NoVariant
  Just _ ->
    isCardPlayable g card
      $> g
        { playedCards = Vector4.set current (Just card) (playedCards g),
          hands = Vector4.modify current (Card.delete card) (hands g)
        }
  where
    g = fromMaybe game (closeTrick game)
    current = currentPlayer g

closeTrick :: GameState -> Maybe GameState
closeTrick g = do
  v <- variant g
  cs <- sequence (playedCards g)
  pure
    g
      { playedCards = Vector4.replicate Nothing,
        tricks = Trick.close v (trickLeader g) cs : tricks g,
        variant = Just $ Variant.next v
      }

closeRound :: GameState -> Maybe GameState
closeRound game = do
  let g = fromMaybe game (closeTrick game)
  v <- variant g
  guard $ all Card.null (hands g)
  let round = Round.close (leader g) (shoved g) v (reverse $ tricks g)
      (newHands, gen) = Card.deal (randomGen g)
  pure
    GameState
      { randomGen = gen,
        rounds = round : rounds g,
        variant = Nothing,
        hands = newHands,
        playedCards = Vector4.replicate Nothing,
        tricks = [],
        leader = leader g + 1,
        shoved = False
      }

view :: Index4 -> GameState -> GameView
view player game =
  GameView
    { variant = variant game,
      hand = Vector4.index player (hands game),
      playedCards = Vector4.rotate player (playedCards game),
      leader = leader game - player,
      currentPlayer = currentPlayer game - player,
      shoved = shoved game
    }

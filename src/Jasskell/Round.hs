module Jasskell.Round
  ( Round,
    Seat,
    start,
    chooser,
    currentPlayer,
    SkipMeldError (..),
    skipMeld,
    ShoveError (..),
    shove,
    ChooseError (..),
    choose,
    PlayError (..),
    playCard,
    Team,
    teamOf,
    forTeam,
    Result (..),
    close,
    total,
    matchBonus,
    resultFor,
    Hand (cards),
    CardStatus (..),
    Violation (..),
    foldHand,
    PlayerView (..),
    playerView,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard, unless, when)
import Data.Fin (Fin)
import Data.Fin qualified as Fin
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Vec (Vec)
import Data.Vec qualified as Vec
import GHC.TypeLits (KnownNat)
import Jasskell.Card (Card, Rank (..), Suit)
import Jasskell.Card qualified as Card
import Jasskell.Card.Seq (CardSeq)
import Jasskell.Card.Seq qualified as CardSeq
import Jasskell.Card.Set (CardSet)
import Jasskell.Card.Set qualified as CardSet
import Jasskell.Meld (Meld)
import Jasskell.Meld qualified as Meld
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
    shoved :: Bool,
    stoeck :: Maybe (Fin n),
    melds :: Vec n [Meld]
  }
  deriving (Show)

data Trick n = Trick
  { cards :: CardSeq,
    leader :: Seat n,
    winner :: Seat n,
    points :: Int
  }
  deriving (Show)

trickCardOf :: (KnownNat n) => Seat n -> Trick n -> Card
trickCardOf seat trick =
  CardSeq.unsafeIndex trick.cards $ Fin.toInt $ seat - trick.leader

-- | Schieber: seats 0 and 2 play against seats 1 and 3 (see 'partner').
type Team = Fin 2

teamOf :: Seat n -> Team
teamOf = Fin.parity

-- | Rotate per-team values so that index 0 is the team of the given seat.
forTeam :: (Vec.Elem a) => Seat n -> Vec 2 a -> Vec 2 a
forTeam seat = Vec.rotate (teamOf seat)

-- | Sum per-seat values into per-team values. Allocates only the result:
-- every team folds over the seats into a plain 'Int'.
perTeam :: (KnownNat n, Vec.Elem a) => (a -> Int) -> Vec n a -> Vec 2 Int
perTeam f v = Vec.generate $ \team ->
  Vec.ifoldl' (\seat acc x -> if teamOf seat == team then acc + f x else acc) 0 v
{-# INLINE perTeam #-}

-- | Points of a finished round per team.
data Result = Result
  { -- | Including the last trick bonus; 157 in total.
    cardPoints :: Vec 2 Int,
    -- | The team that won every trick, if any. It gets 'matchBonus'.
    match :: Maybe Team,
    meldPoints :: Vec 2 Int,
    stoeckPoints :: Vec 2 Int
  }
  deriving (Eq, Show)

-- | Points per team, including the match bonus.
total :: Result -> Vec 2 Int
total r = Vec.generate $ \team ->
  Vec.index r.cardPoints team
    + Vec.index r.meldPoints team
    + Vec.index r.stoeckPoints team
    + if r.match == Just team then matchBonus else 0

-- | Rotate a result so that index 0 is the team of the given seat.
resultFor :: Seat n -> Result -> Result
resultFor seat r =
  Result
    { cardPoints = forTeam seat r.cardPoints,
      match = subtract (teamOf seat) <$> r.match,
      meldPoints = forTeam seat r.meldPoints,
      stoeckPoints = forTeam seat r.stoeckPoints
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
      shoved = False,
      stoeck = Nothing,
      melds = Vec.replicate []
    }

-- | The seat that declares the variant (or shoves) in this round.
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
playedCards round =
  Vec.generate $
    \i -> CardSeq.index round.played (Fin.toInt $ i + l)
  where
    !l = leader round

trickWinner :: (KnownNat n) => Round n -> Maybe (Fin n)
trickWinner round = do
  v <- round.variant
  c <- CardSeq.index round.played 0
  let !lead = Card.suit c
      !highest = CardSeq.maxIndexBy (Card.compare lead v) round.played
  pure $! Fin.unsafe highest + leader round

data ShoveError
  = CannotShoveNow
  | AlreadyShoved
  deriving (Eq, Show)

shove :: Round n -> Either ShoveError (Round n)
shove round
  | isJust round.variant = Left CannotShoveNow
  | round.shoved = Left AlreadyShoved
  | otherwise = Right $! round {shoved = True, hands = round.hands}

data ChooseError = CannotChooseNow
  deriving (Eq, Show)

choose :: (KnownNat n) => Variant -> Round n -> Either ChooseError (Round n)
choose v round
  | isJust round.variant = Left CannotChooseNow
  | otherwise =
      Right $!
        round
          { variant = Just v,
            melds = Vec.map (Meld.melds v) round.hands,
            hands = round.hands
          }

data SkipMeldError = AlreadyAnnounced

skipMeld :: (KnownNat n) => Seat n -> Round n -> Either SkipMeldError (Round n)
skipMeld seat round
  | null round.tricks,
    isNothing (Vec.index (playedCards round) seat) =
      Right $! round {melds = Vec.set seat [] round.melds, hands = round.hands}
  | otherwise = Left AlreadyAnnounced

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
          hands = Vec.set current (CardSet.delete card hand) round.hands,
          stoeck =
            round.stoeck <|> do
              guard $ isStoeckCard v card
              guard $ any (isStoeckCard v . trickCardOf current) round.tricks
              pure current
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
            points =
              CardSeq.foldl' (\p c -> p + Card.points v c) 0 cards
                + if lastTrick then lastTrickBonus else 0
          }
      -- 'playCard' removes the card from the hand before closing the trick.
      lastTrick = Vec.all CardSet.null round.hands
      melds =
        Vec.ifoldr (\i ms rest -> case ms of m : _ -> (i + l, m) : rest; _ -> rest) [] $
          Vec.rotate l round.melds
      meldWinner
        | null round.tricks = case melds of
            [] -> Nothing
            m : ms -> Just . fst $ foldl' (Meld.betterOn snd v) m ms
        | otherwise = Nothing
  pure $!
    round
      { played = CardSeq.empty,
        tricks = trick : round.tricks,
        variant = Just $ Variant.next v,
        points = Vec.modify trick.winner (+ trick.points) round.points,
        melds = case meldWinner of
          Nothing -> round.melds
          Just w -> Vec.imap (\i ms -> if i == w || i == partner w then ms else []) round.melds
      }

lastTrickBonus, matchBonus, stoeckBonus :: Int
lastTrickBonus = 5
matchBonus = 100
stoeckBonus = 20

-- | The result of the round, once all cards are played.
close :: (KnownNat n) => Round n -> Maybe Result
close round = do
  guard $ Vec.all CardSet.null round.hands
  pure
    Result
      { cardPoints = perTeam id round.points,
        match = case round.tricks of
          t : ts
            | all (\t' -> teamOf t'.winner == teamOf t.winner) ts ->
                Just $ teamOf t.winner
          _ -> Nothing,
        -- TODO: only count melds if the team won at least one trick
        meldPoints = perTeam (sum . map Meld.points) round.melds,
        stoeckPoints = Vec.generate $ \team ->
          if fmap teamOf round.stoeck == Just team then stoeckBonus else 0
      }

isStoeckCard :: Variant -> Card -> Bool
isStoeckCard v c = case v of
  Trump t -> Card.suit c == t && let r = Card.rank c in r == Over || r == King
  _ -> False

-- TODO: make it work for non schieber games.
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

data PlayerView n = PlayerView
  { chooser :: Seat n,
    leader :: Seat n,
    currentPlayer :: Seat n,
    hand :: Hand,
    variant :: Maybe Variant,
    shoved :: Bool,
    playedCards :: Vec n (Maybe Card),
    -- | Card points per team so far; index 0 is the viewer's team.
    points :: Vec 2 Int,
    trickWinner :: Maybe (Fin n),
    stoeck :: Maybe (Fin n),
    melds :: Vec n [Meld],
    announcments :: Vec n (Maybe Meld)
  }
  deriving (Show)

playerView :: (KnownNat n) => Round n -> Fin n -> PlayerView n
playerView round seat =
  PlayerView
    { chooser = round.chooser - seat,
      leader = leader round - seat,
      currentPlayer = currentPlayer round - seat,
      hand = handOf seat round,
      variant = round.variant,
      shoved = round.shoved,
      playedCards = Vec.rotate seat $ playedCards round,
      points = forTeam seat $ perTeam id round.points,
      trickWinner = trickWinner round <&> subtract seat,
      stoeck = round.stoeck <&> subtract seat,
      melds =
        if null round.tricks
          then Vec.set 0 (Vec.index round.melds seat) (Vec.replicate [])
          else Vec.rotate seat round.melds,
      announcments =
        if null round.tricks
          then Vec.rotate seat $ Vec.zipWith (liftA2 const . listToMaybe) round.melds (playedCards round)
          else Vec.replicate Nothing
    }

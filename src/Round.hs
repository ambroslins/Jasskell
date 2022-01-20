{-# LANGUAGE FlexibleContexts #-}

module Round
  ( Round (hands, leader, cards, tricks, variant),
    Record,
    Error (..),
    Result (..),
    playCard,
    current,
  )
where

import Card (Card, Cards)
import Card qualified
import Data.Finite (Finite, modulo)
import Data.Set qualified as Set
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import JassNat (JassNat)
import Relude.Extra.Lens (over)
import Round.Record (Record)
import Round.Record qualified as Record
import Trick (Trick)
import Trick qualified
import Variant (Variant)
import Variant qualified
import Prelude hiding (round)

data Round n = Round
  { hands :: Vector n Cards,
    leader :: Finite n,
    cards :: [Card],
    tricks :: [Trick n],
    variant :: Variant
  }
  deriving (Show)

data Error
  = NotYourTurn
  | CardUnplayable Card.Reason
  deriving (Show)

data Result n
  = Ok (Round n)
  | Closed (Record n)
  | Error Error
  deriving (Show)

playCard :: JassNat n => Finite n -> Card -> Round n -> Result n
playCard player card round
  | player /= current round = Error NotYourTurn
  | otherwise = case Card.status (variant round) (cards round) (Vector.index (hands round) player) card of
    Card.Unplayable reason -> Error (CardUnplayable reason)
    Card.Playable ->
      case Trick.make (variant round) (leader round) cards' of
        Nothing ->
          Ok $
            Round
              { hands = hands',
                leader = leader round + 1,
                cards = cards',
                tricks = tricks round,
                variant = variant round
              }
        Just trick ->
          maybe
            (Ok round')
            (Closed . Record.make)
            (Vector.fromList $ reverse tricks')
          where
            winner = Trick.winner trick
            tricks' = trick : tricks round
            round' =
              Round
                { hands = hands',
                  leader = winner,
                  cards = [],
                  tricks = tricks',
                  variant = Variant.next $ variant round
                }
      where
        cards' = cards round ++ [card]
        hands' = over (Vector.ix player) (Set.delete card) (hands round)

current :: KnownNat n => Round n -> Finite n
current r = leader r + modulo (toInteger $ length $ cards r)

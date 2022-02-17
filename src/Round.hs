module Round
  ( Round,
    Error (..),
    Record,
    playCard,
    current,
    chooseVariant,
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
import Trick (Trick)
import Trick qualified
import Variant (Variant)
import Variant qualified
import Prelude hiding (round)

data Round n = Round
  { hands :: Vector n Cards,
    leader :: Finite n,
    phase :: Phase n
  }
  deriving (Show)

data Phase n
  = Choosing
  | Playing (RoundPlaying n)
  deriving (Show)

data RoundPlaying n = RoundPlaying
  { variant :: Variant,
    cards :: [Card],
    tricks :: [Trick n]
  }
  deriving (Show)

data Error
  = ChooseVariantFirst
  | VariantAlreadyDefined
  | CardUnplayable Card.Reason
  deriving (Show)

current :: KnownNat n => Round n -> Finite n
current Round {leader, phase} = case phase of
  Choosing -> leader
  Playing RoundPlaying {cards} -> leader + modulo (toInteger $ length cards)

playCard :: JassNat n => Card -> Round n -> Either Error (Round n)
playCard card round@Round {hands, leader, phase} = case phase of
  Choosing -> Left ChooseVariantFirst
  Playing playing@RoundPlaying {variant, cards, tricks} ->
    case Card.status variant cards (Vector.index hands player) card of
      Card.Unplayable reason -> Left (CardUnplayable reason)
      Card.Playable ->
        pure $ case Trick.make variant leader cards' of
          Nothing ->
            round
              { hands = hands',
                phase =
                  Playing
                    playing
                      { cards = cards'
                      }
              }
          Just trick ->
            round
              { hands = hands',
                leader = Trick.winner trick,
                phase =
                  Playing
                    playing
                      { variant = Variant.next variant,
                        cards = cards',
                        tricks = trick : tricks
                      }
              }
    where
      player = current round
      cards' = cards ++ [card]
      hands' = over (Vector.ix player) (Set.delete card) hands

chooseVariant :: JassNat n => Variant -> Round n -> Either Error (Round n)
chooseVariant variant round@Round {phase} = case phase of
  Choosing ->
    pure $
      round
        { phase = Playing $ RoundPlaying variant [] []
        }
  Playing _ -> Left VariantAlreadyDefined

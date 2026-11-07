module Jasskell.View
  ( Waiting (..),
    makeWaiting,
    Playing (..),
    makePlaying,
    Spectating (..),
    makeSpectating,
  )
where

import Data.Fin (Fin)
import Data.Vec (Vec)
import Data.Vec qualified as Vec
import GHC.TypeLits (KnownNat)
import Jasskell.Card (Card)
import Jasskell.Player (Player (..), PlayerId)
import Jasskell.Round (Round, Seat)
import Jasskell.Round qualified as Round
import Jasskell.Variant (Variant)
import Prelude hiding (round)

data Waiting n = Waiting
  { seats :: Vec n (Maybe Player),
    seated :: Bool
  }
  deriving (Show)

makeWaiting :: (KnownNat n) => Vec n (Maybe Player) -> PlayerId -> Waiting n
makeWaiting seats playerId = case Vec.findIndex hasSeat seats of
  Nothing -> Waiting {seats, seated = False}
  Just i -> Waiting {seats = Vec.rotate i seats, seated = True}
  where
    hasSeat = maybe False (\p -> p.id == playerId)

data Playing n = Playing
  { players :: Vec n Player,
    chooser :: Seat n,
    leader :: Seat n,
    currentPlayer :: Seat n,
    hand :: Round.Hand,
    variant :: Maybe Variant,
    shoved :: Bool,
    playedCards :: Vec n (Maybe Card)
  }
  deriving (Show)

makePlaying :: (KnownNat n) => Vec n Player -> Round n -> Fin n -> Playing n
makePlaying players round seat =
  Playing
    { players = Vec.rotate seat players,
      chooser = Round.chooser round - seat,
      leader = Round.leader round - seat,
      currentPlayer = Round.currentPlayer round - seat,
      hand = Round.handOf seat round,
      variant = Round.variant round,
      shoved = Round.shoved round,
      playedCards = Vec.rotate seat $ Round.playedCards round
    }

newtype Spectating n = Spectating
  { players :: Vec n Player
  }
  deriving (Show)

makeSpectating :: Vec n Player -> Spectating n
makeSpectating = Spectating

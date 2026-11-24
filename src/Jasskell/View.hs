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
import Jasskell.Player (Player (..), PlayerId)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
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

-- | Everything is rotated to the viewer: seat 0 and team 0 are their own.
data Playing n = Playing
  { players :: Vec n Player,
    -- | Total points per team of the finished rounds before this one.
    score :: Vec 2 Int,
    round :: Round.PlayerView n,
    -- | The result once all cards of this round are played.
    result :: Maybe Round.Result
  }
  deriving (Show)

makePlaying :: (KnownNat n) => Vec n Player -> Vec 2 Int -> Round n -> Fin n -> Playing n
makePlaying players score round seat =
  Playing
    { players = Vec.rotate seat players,
      score = Round.forTeam seat score,
      round = Round.playerView round seat,
      result = Round.resultFor seat <$> Round.close round
    }

newtype Spectating n = Spectating
  { players :: Vec n Player
  }
  deriving (Show)

makeSpectating :: Vec n Player -> Spectating n
makeSpectating = Spectating

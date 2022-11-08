module Jasskell.Server.GuestView
  ( GuestView,
    makeJoining,
    makeSpectating,
    encoder,
  )
where

import Data.Aeson.Combinators.Encode (Encoder, field)
import Data.Aeson.Combinators.Encode qualified as Encoder hiding (vector)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Server.Encoder (Tagged (Tagged))
import Jasskell.Server.Encoder qualified as Encoder
import Jasskell.Server.Seat (Seat (..))
import Jasskell.Server.Seat qualified as Seat

data GuestView n = MakeGuestView
  { seats :: Vector n Seat,
    phase :: Phase
  }
  deriving (Show)

data Phase
  = Joining
  | Spectating
  deriving (Show)

make :: Phase -> Vector n Seat -> GuestView n
make p ss =
  MakeGuestView
    { seats = ss,
      phase = p
    }

makeJoining :: Vector n Seat -> GuestView n
makeJoining = make Joining

makeSpectating :: Vector n Seat -> GuestView n
makeSpectating = make Spectating

encoder :: Encoder (GuestView n)
encoder =
  Encoder.object
    [ field "seats" (Encoder.vector Seat.encoder) seats,
      field "phase" phaseEncoder phase
    ]
  where
    phaseEncoder :: Encoder Phase
    phaseEncoder = Encoder.tagged $ \case
      Joining -> Tagged "joining" Encoder.unit ()
      Spectating -> Tagged "spectating" Encoder.unit ()
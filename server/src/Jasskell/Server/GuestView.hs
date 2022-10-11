module Jasskell.Server.GuestView
  ( GuestView,
    makeWaiting,
    makeActive,
    encoder,
  )
where

import Data.Aeson.Combinators.Encode (Encoder, field)
import Data.Aeson.Combinators.Encode qualified as Encoder hiding (vector)
import Data.Text qualified as Text
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Jasskell.Server.Encoder qualified as Encoder
import Jasskell.Server.Seat (Seat (..))
import Jasskell.Server.Seat qualified as Seat
import Jasskell.Server.User (User)

data GuestView n = MakeGuestView
  { seats :: Vector n Seat,
    phase :: Phase
  }
  deriving (Show)

data Phase
  = Waiting
  | Active
  deriving (Show)

make :: Phase -> Vector n (Maybe User) -> GuestView n
make p us =
  MakeGuestView
    { seats = Vector.map (maybe Empty Taken) us,
      phase = p
    }

makeWaiting :: Vector n (Maybe User) -> GuestView n
makeWaiting = make Waiting

makeActive :: Vector n (Maybe User) -> GuestView n
makeActive = make Active

encoder :: Encoder (GuestView n)
encoder =
  Encoder.object
    [ field "status" Encoder.text (const "guest"),
      field "seats" (Encoder.vector Seat.encoder) seats,
      field "phase" Encoder.text (Text.toLower . show . phase)
    ]
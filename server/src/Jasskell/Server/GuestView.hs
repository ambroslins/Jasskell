module Jasskell.Server.GuestView
  ( GuestView,
    makeWaiting,
    makeActive,
    encoder,
  )
where

import Data.Aeson.Combinators.Encode (Encoder, field)
import Data.Aeson.Combinators.Encode qualified as Encoder hiding (vector)
import Data.Vector.Sized (Vector)
import Jasskell.Server.Encoder qualified as Encoder
import Jasskell.Server.User (User)

data GuestView n = MakeGuestView
  { users :: Vector n (Maybe User),
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
    { users = us,
      phase = p
    }

makeWaiting :: Vector n (Maybe User) -> GuestView n
makeWaiting = make Waiting

makeActive :: Vector n (Maybe User) -> GuestView n
makeActive = make Active

encoder :: Encoder (GuestView n)
encoder =
  Encoder.object
    [ field "users" (Encoder.vector (Encoder.nullable Encoder.user)) users,
      field "phase" Encoder.text (show . phase)
    ]
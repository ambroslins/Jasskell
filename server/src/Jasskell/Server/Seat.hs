module Jasskell.Server.Seat
  ( Seat (..),
    encoder,
  )
where

import Data.Aeson.Combinators.Encode (Encoder)
import Jasskell.Server.Encoder qualified as Encoder
import Jasskell.Server.User (User)
import Jasskell.Server.User qualified as User

data Seat
  = Empty
  | Taken User
  deriving (Eq, Show)

toMaybe :: Seat -> Maybe User
toMaybe = \case
  Empty -> Nothing
  Taken user -> Just user

encoder :: Encoder Seat
encoder = contramap toMaybe (Encoder.nullable User.encoder)
module Jasskell.Server.User
  ( User (..),
    encoder,
  )
where

import Data.Aeson.Combinators.Encode (Encoder, field, object)
import Data.Aeson.Combinators.Encode qualified as Encoder

newtype User = User
  { name :: Text
  }
  deriving (Eq, Show)

encoder :: Encoder User
encoder =
  object
    [ field "name" Encoder.text name
    ]

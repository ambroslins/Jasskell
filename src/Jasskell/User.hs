module Jasskell.User (UserId, User (..)) where

import Data.Text (Text)
import Jasskell.Id (Id)

type UserId = Id User

data User = User
  { id :: !UserId,
    name :: !Text
  }

module Jasskell.Server.User
  ( User (..),
  )
where

newtype User = User
  { name :: Text
  }
  deriving (Eq, Show)

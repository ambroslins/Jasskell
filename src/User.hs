module User where

newtype User = User {name :: Text}
  deriving (Show)
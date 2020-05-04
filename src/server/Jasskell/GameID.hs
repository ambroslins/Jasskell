{-# LANGUAGE DeriveGeneric #-}

module Jasskell.GameID where

import           Data.Aeson
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )
import           GHC.Generics

newtype GameID = GameID UUID deriving (Eq, Ord, Show, Generic)

instance ToJSON GameID where

instance FromJSON GameID where

newGameID :: IO GameID
newGameID = GameID <$> nextRandom

module Jasskell.GameID where

import           Data.Aeson
import           Data.UUID                      ( UUID
                                                , toString
                                                , toText
                                                , fromText
                                                )
import           Data.UUID.V4                   ( nextRandom )

newtype GameID = GameID UUID deriving (Eq, Ord)

instance Show GameID where
    show (GameID uuid) = toString uuid

instance ToJSON GameID where
    toJSON (GameID uuid) = String $ toText uuid

instance FromJSON GameID where
    parseJSON = withText "gameID" $ \t -> case fromText t of
        Just g  -> pure $ GameID g
        Nothing -> fail "not a GameID"

newGameID :: IO GameID
newGameID = GameID <$> nextRandom

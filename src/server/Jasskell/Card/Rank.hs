{-# LANGUAGE OverloadedStrings #-}

module Jasskell.Card.Rank where

import           Data.Aeson
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Text.Read                      ( readMaybe )

data Rank = Six | Seven | Eight | Nine | Ten | Under | Over | King | Ace
    deriving(Eq, Ord, Bounded, Enum, Show, Read)

instance ToJSON Rank where
    toJSON = String . pack . show

instance FromJSON Rank where
    parseJSON (String t) = case readMaybe $ unpack t of
        Just r  -> pure r
        Nothing -> fail ""
    parseJSON _ = fail ""

{-# LANGUAGE OverloadedStrings #-}

module Jasskell.Card.Suit where

import           Data.Aeson
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Text.Read                      ( readMaybe )

data Suit = Bells | Hearts | Acorns | Leaves
    deriving(Eq, Ord, Bounded, Enum, Show, Read)

instance ToJSON Suit where
    toJSON = String . pack . show

instance FromJSON Suit where
    parseJSON (String t) = case readMaybe $ unpack t of
        Just s  -> pure s
        Nothing -> fail ""
    parseJSON _ = fail ""


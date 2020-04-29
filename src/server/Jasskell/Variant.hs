{-# LANGUAGE OverloadedStrings #-}

module Jasskell.Variant where

import           Data.Aeson
import           Data.Foldable                  ( asum )
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Jasskell.Card.Suit
import           Text.Read                      ( readMaybe )

data Variant = Trump Suit | Direction Direction | Slalom Direction
    deriving (Eq, Show)

data Direction = BottomUp | TopDown deriving (Eq, Show, Read)

nextVariant :: Variant -> Variant
nextVariant (Slalom TopDown ) = Slalom BottomUp
nextVariant (Slalom BottomUp) = Slalom TopDown
nextVariant v                 = v

instance ToJSON Direction where
    toJSON = String . pack . show

instance FromJSON Direction where
    parseJSON (String t) = case readMaybe $ unpack t of
        Just a  -> pure a
        Nothing -> fail ""
    parseJSON _ = fail ""

instance ToJSON Variant where
    toJSON (Trump     t  ) = object ["trump" .= t]
    toJSON (Direction dir) = object ["direction" .= dir]
    toJSON (Slalom    dir) = object ["slalom" .= dir]

instance FromJSON Variant where
    parseJSON = withObject "variant" $ \o -> asum
        [ Trump <$> o .: "trump"
        , Direction <$> o .: "direction"
        , Slalom <$> o .: "slalom"
        ]

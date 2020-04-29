{-# LANGUAGE OverloadedStrings #-}

module Jasskell.Action where

import           Data.Aeson
import           Data.Foldable                  ( asum )
import           Jasskell.Card
import           Jasskell.Variant

data Action = PlayCard Card
            | ChooseVariant Variant
            deriving (Show)

instance FromJSON Action where
    parseJSON = withObject "action" $ \o ->
        asum
            [ PlayCard <$> o .: "playCard"
            , ChooseVariant <$> o .: "chooseVariant"
            ]

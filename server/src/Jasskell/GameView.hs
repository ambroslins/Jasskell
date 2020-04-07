{-# LANGUAGE OverloadedStrings #-}

module Jasskell.GameView where

import           Data.Aeson
import           Jasskell.Card
import           Jasskell.Variant

data GameView = GameView { hand :: Cards
                         , table :: [(String, Maybe Card)]
                         , variantView :: Maybe Variant
                         }

instance Show GameView where
    show gv = unlines [show $ variantView gv, show $ table gv, show $ hand gv]


instance ToJSON GameView where
    toJSON gv = object
        [ "hand" .= hand gv
        , "table" .= map snd (table gv)
        -- , "variant" .= variantView gv
        ]

{-# LANGUAGE OverloadedStrings #-}

module Jasskell.GameView where

import Data.Aeson
import Jasskell.Card
import Jasskell.Variant

data HandCard = HandCard
  { card :: Card,
    playable :: Bool
  }
  deriving (Show)

data GameView = GameView
  { hand :: [HandCard],
    table :: [(String, Maybe Card)],
    variantView :: Maybe Variant
  }

-- TODO

instance Show GameView where
  show gv = unlines [show $ variantView gv, show $ table gv, show $ hand gv]

instance ToJSON HandCard where
  toJSON hc = object ["card" .= card hc, "playable" .= playable hc]

instance ToJSON GameView where
  toJSON gv =
    object
      [ "hand" .= hand gv,
        "table" .= map snd (table gv)
        -- , "variant" .= variantView gv
      ]

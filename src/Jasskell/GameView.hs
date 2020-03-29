module Jasskell.GameView where

import           Jasskell.Card
import           Jasskell.Variant

data GameView = GameView { hand :: Cards
                         , table :: [(String, Maybe Card)]
                         , variantView :: Maybe Variant
                         }

instance Show GameView where
    show gv = unlines [show $ variantView gv, show $ table gv, show $ hand gv]

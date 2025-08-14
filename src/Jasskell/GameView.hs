module Jasskell.GameView
  ( GameView (..),
  )
where

import Data.Vector4 (Index4, Vector4)
import Jasskell.Card (Card, CardSet)
import Jasskell.Variant (Variant)
import Prelude hiding (round)

data GameView = GameView
  { variant :: !(Maybe Variant),
    hand :: !CardSet,
    playedCards :: !(Vector4 (Maybe Card)),
    leader :: !Index4,
    currentPlayer :: !Index4,
    shoved :: !Bool
  }
  deriving (Show)

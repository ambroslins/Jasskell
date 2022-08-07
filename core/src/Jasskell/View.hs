module Jasskell.View
  ( Views,
    Declaring,
    Playing,
    PlayableCard,
    SomeView (..),
  )
where

import Data.Finite (Finite)
import Jasskell.View.Declaring (Declaring)
import Jasskell.View.Playing (PlayableCard, Playing)

type Views view n = Finite n -> view n

data SomeView n
  = ViewPlaying (Playing n)
  | ViewDeclaring (Declaring n)
  deriving (Show)

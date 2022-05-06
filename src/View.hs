module View
  ( Views,
    Declaring,
    Playing,
    PlayableCard,
    SomeView (..),
  )
where

import Data.Finite (Finite)
import View.Declaring (Declaring)
import View.Playing (PlayableCard, Playing)

type Views view n = Finite n -> view n

data SomeView n
  = ViewPlaying (Playing n)
  | ViewDeclaring (Declaring n)
  deriving (Show)

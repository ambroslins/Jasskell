module View
  ( Views,
    Declaring,
    Playing,
    PlayableCard,
  )
where

import Data.Finite (Finite)
import View.Declaring (Declaring)
import View.Playing (PlayableCard, Playing)

type Views view n = Finite n -> view n

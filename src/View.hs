module View
  ( Views,
    Declaring,
    Playing,
  )
where

import Data.Finite (Finite)
import View.Declaring (Declaring)
import View.Playing (Playing)

type Views view n = Finite n -> view n

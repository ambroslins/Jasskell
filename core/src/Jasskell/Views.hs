module Jasskell.Views
  ( Views,
    make,
    pov,
  )
where

import Data.Finite (Finite)

newtype Views view n = Views {pov :: Finite n -> view n}

make :: (Finite n -> view n) -> Views view n
make = Views

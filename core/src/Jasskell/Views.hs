module Jasskell.Views
  ( Views,
    make,
    pov,
    map,
  )
where

import Data.Finite (Finite)
import Prelude hiding (map)

newtype Views view n = Views {pov :: Finite n -> view n}

make :: (Finite n -> view n) -> Views view n
make = Views

map :: (Finite n -> view n -> view' n) -> Views view n -> Views view' n
map f (Views vs) = Views $ \i -> f i (vs i)

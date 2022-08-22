module Jasskell.Interface
  ( Interface (..),
  )
where

import Data.Finite (Finite)
import Jasskell.Card (Card)
import Jasskell.View.Playing (Playing)
import Jasskell.Views (Views)

data Interface n m = Interface
  { promptCard :: Finite n -> Views Playing n -> m Card
  }

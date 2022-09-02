module Jasskell.Jass (JassNat) where

import GHC.TypeLits (Div, type (+), type (-))

type JassNat n = (KnownNat n, KnownNat (Div 36 n), n ~ ((n - 1) + 1))

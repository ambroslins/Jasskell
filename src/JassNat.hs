module JassNat where

import GHC.TypeLits (Div, KnownNat, type (+), type (-))

type JassNat n = (KnownNat n, KnownNat (Div 36 n), n ~ ((n - 1) + 1))
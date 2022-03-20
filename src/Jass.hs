{-# LANGUAGE TemplateHaskell #-}

module Jass where

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import GHC.TypeLits (Div, KnownNat, type (+), type (-))
import Variant (Variant)
import View (Declaring, PlayableCard, Playing, View)

data Jass n next
  = PromptCard (View Playing n) (PlayableCard -> next)
  | PromptVariant (View Declaring n) (Variant -> next)
  deriving (Functor)

makeFree ''Jass

type JassNat n = (KnownNat n, KnownNat (Div 36 n), n ~ ((n - 1) + 1))

type MonadJass n m = (JassNat n, MonadFree (Jass n) m)

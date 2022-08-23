module Jasskell.Declaration
  ( Declaration (..),
    BadDeclaration,
    Interface (..),
    declareVariant,
  )
where

import Data.Finite (Finite)
import Jasskell.Variant (Variant)

data Declaration
  = Choose Variant
  | Shove
  deriving (Eq, Show)

data BadDeclaration
  = NobodyToShove
  deriving (Eq, Show)

data Interface n m = Interface
  { promptDeclaration :: Finite n -> [Finite n] -> m Declaration,
    throwBadDeclaration :: forall a. BadDeclaration -> m a
  }

declareVariant ::
  forall n m.
  Monad m =>
  Interface n m ->
  NonEmpty (Finite n) ->
  m Variant
declareVariant interface@Interface {..} (p :| ps) = do
  declaration <- promptDeclaration p ps
  case declaration of
    Choose variant -> pure variant
    Shove ->
      maybe
        (throwBadDeclaration NobodyToShove)
        (declareVariant interface)
        $ nonEmpty ps

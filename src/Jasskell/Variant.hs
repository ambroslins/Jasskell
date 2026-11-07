{-# LANGUAGE TemplateHaskell #-}

module Jasskell.Variant
  ( Variant (..),
    Direction (..),
    next,
    toInt16,
    fromInt16,
  )
where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int16)
import Data.Vector qualified as V
import Jasskell.Card.Internal (Suit)

data Variant
  = Trump Suit
  | Direction Direction
  | Slalom Direction
  deriving (Eq, Show)

data Direction = BottomUp | TopDown
  deriving (Eq, Show)

next :: Variant -> Variant
next = \case
  Slalom BottomUp -> Slalom TopDown
  Slalom TopDown -> Slalom BottomUp
  v -> v

toInt16 :: Variant -> Int16
toInt16 = \case
  Trump t -> fromIntegral $ fromEnum t
  Direction dir -> 4 + directionToInt16 dir
  Slalom dir -> 6 + directionToInt16 dir
  where
    directionToInt16 = \case
      BottomUp -> 0
      TopDown -> 1

fromInt16 :: Int16 -> Maybe Variant
fromInt16 i = variants V.!? fromIntegral i

variants :: V.Vector Variant
variants =
  V.fromList $
    map Trump [minBound .. maxBound]
      <> ([Direction, Slalom] <*> [BottomUp, TopDown])

$(deriveJSON defaultOptions ''Direction)
$(deriveJSON defaultOptions ''Variant)

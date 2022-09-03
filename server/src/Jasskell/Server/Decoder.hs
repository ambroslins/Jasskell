module Jasskell.Server.Decoder where

import Data.Aeson (Key)
import Data.Aeson.Combinators.Decode (Decoder, key)
import Data.Aeson.Combinators.Decode qualified as Decoder
import Data.HashMap.Strict qualified as HashMap
import Jasskell.Card (Card (Card), Rank, Suit)
import Jasskell.Declaration (Declaration (..))
import Jasskell.Server.Action (Action (..))
import Jasskell.Server.GameState (Move (..))
import Jasskell.Variant (Direction (..), Variant (..))
import Relude.Extra (toFst)

withLookupTable ::
  (Show b, Eq b, Hashable b) =>
  [a] ->
  (a -> b) ->
  Decoder b ->
  Decoder a
withLookupTable values f decoder = do
  x <- decoder
  case HashMap.lookup x lookupTable of
    Nothing -> fail $ "Invalid enum value" <> show x
    Just y -> pure y
  where
    lookupTable = HashMap.fromList $ map (toFst f) values

boundedEnum ::
  (Enum a, Bounded a, Show b, Eq b, Hashable b) =>
  (a -> b) ->
  Decoder b ->
  Decoder a
boundedEnum = withLookupTable [minBound .. maxBound]

suit :: Decoder Suit
suit = boundedEnum show Decoder.text

rank :: Decoder Rank
rank = boundedEnum show Decoder.text

card :: Decoder Card
card = Card <$> key "suit" suit <*> key "rank" rank

direction :: Decoder Direction
direction = withLookupTable [TopDown, BottomUp] show Decoder.text

data Tagged a = forall b. Tagged Key (b -> a) (Decoder b)

tagged :: [Tagged a] -> Decoder a
tagged = asum . map (\(Tagged tag f decoder) -> f <$> key tag decoder)

variant :: Decoder Variant
variant =
  tagged
    [ Tagged "trump" Trump suit,
      Tagged "direction" Direction direction,
      Tagged "slalom" Slalom direction
    ]

declaration :: Decoder Declaration
declaration = maybe Shove Choose <$> Decoder.nullable variant

move :: Decoder Move
move =
  tagged
    [ Tagged "playCard" PlayCard card,
      Tagged "declare" Declare declaration
    ]

action :: Decoder Action
action =
  tagged
    [ Tagged "move" Move move,
      Tagged "start" (const StartGame) Decoder.unit
    ]

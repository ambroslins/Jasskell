module Jasskell.Server.Decoder where

import Data.Aeson (Key)
import Data.Aeson.Combinators.Decode (Decoder, key)
import Data.Aeson.Combinators.Decode qualified as Decoder
import Data.Finite (Finite, packFinite)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Jasskell.Card (Card (Card), Rank, Suit)
import Jasskell.Declaration (Declaration (..))
import Jasskell.Jass (JassNat)
import Jasskell.Server.Action (Action)
import Jasskell.Server.Action qualified as Action
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

boundedEnumShowLowerCase :: (Enum a, Bounded a, Show a) => Decoder a
boundedEnumShowLowerCase = boundedEnum (Text.toLower . show) Decoder.text

finite :: KnownNat n => Decoder (Finite n)
finite =
  -- TODO: use toBoundedInteger from scientific
  Decoder.integer >>= maybe (fail "Out of bounds") pure . packFinite

suit :: Decoder Suit
suit = boundedEnumShowLowerCase

rank :: Decoder Rank
rank = boundedEnumShowLowerCase

card :: Decoder Card
card = Card <$> key "suit" suit <*> key "rank" rank

direction :: Decoder Direction
direction =
  withLookupTable
    [TopDown, BottomUp]
    (Text.toLower . show)
    Decoder.text

data Tagged a = Tagged Key (Decoder a)

tagged :: [Tagged a] -> Decoder a
tagged = asum . map (\(Tagged tag decoder) -> key tag decoder)

variant :: Decoder Variant
variant =
  tagged
    [ Tagged "trump" $ Trump <$> suit,
      Tagged "direction" $ Direction <$> direction,
      Tagged "slalom" $ Slalom <$> direction
    ]

declaration :: Decoder Declaration
declaration = maybe Shove Choose <$> Decoder.nullable variant

move :: Decoder Move
move =
  tagged
    [ Tagged "play-card" $ PlayCard <$> card,
      Tagged "declare" $ Declare <$> declaration
    ]

action :: JassNat n => Decoder (Action n)
action =
  tagged
    [ Tagged "move" $ Action.PlayMove <$> move,
      Tagged "start" $ Action.StartGame <$ Decoder.unit,
      Tagged "take-seat" $
        Action.TakeSeat
          <$> key "username" Decoder.text
          <*> key "seat" finite
    ]

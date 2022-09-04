module Jasskell.Server.TableID
  ( TableID,
    new,
    toText,
    fromText,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Prelude hiding (toText)

newtype TableID = TableID UUID
  deriving newtype (Eq, Ord, Hashable, Show, ToJSON, FromJSON)

new :: IO TableID
new = TableID <$> UUID.V4.nextRandom

toText :: TableID -> Text
toText = coerce UUID.toText

fromText :: Text -> Maybe TableID
fromText = coerce UUID.fromText

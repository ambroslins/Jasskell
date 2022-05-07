module TableID
  ( TableID,
    new,
    toText,
    fromText,
  )
where

import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Prelude hiding (toText)

newtype TableID = TableID UUID
  deriving newtype (Eq, Ord, Hashable, Show)

new :: IO TableID
new = TableID <$> UUID.V4.nextRandom

toText :: TableID -> Text
toText (TableID uuid) = UUID.toText uuid

fromText :: Text -> Maybe TableID
fromText uuid = TableID <$> UUID.fromText uuid

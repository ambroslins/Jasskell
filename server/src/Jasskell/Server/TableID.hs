module Jasskell.Server.TableID
  ( TableID,
    new,
    toText,
    fromText,
    toByteString,
    fromByteString,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Web.Twain (HttpError (HttpError), ParsableParam (parseParam), status400)
import Prelude hiding (toText)

newtype TableID = TableID UUID
  deriving newtype (Eq, Ord, Hashable, Show, ToJSON, FromJSON)

instance ParsableParam TableID where
  parseParam =
    maybe
      (Left (HttpError status400 "parseParam TableId: no parse"))
      pure
      . fromText

new :: MonadIO m => m TableID
new = TableID <$> liftIO UUID.V4.nextRandom

toText :: TableID -> Text
toText = coerce UUID.toText

fromText :: Text -> Maybe TableID
fromText = coerce UUID.fromText

toByteString :: TableID -> ByteString
toByteString = coerce UUID.toASCIIBytes

fromByteString :: ByteString -> Maybe TableID
fromByteString = coerce UUID.fromASCIIBytes
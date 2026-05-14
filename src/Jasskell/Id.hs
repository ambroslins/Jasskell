module Jasskell.Id
  ( Id (..),
    toInt,
    fromInt,
    encodeByteString,
    encodeText,
    decodeText,
    decodeByteString,
    new,
    utcTime,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.Binary.Get qualified as Get
import Data.Binary.Put qualified as Put
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL as Base64
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Clock.System (SystemTime (..), getSystemTime, systemToUTCTime)
import Data.Word (Word64)
import System.Random.Stateful qualified as Random
import Web.Twain (HttpError (..), ParsableParam (..), badRequest400)

newtype Id a = Id Int
  deriving (Eq, Show)

toInt :: Id a -> Int
toInt (Id i) = i

fromInt :: Int -> Id a
fromInt = Id

word64ToByteString :: Word64 -> ByteString
word64ToByteString = BS.toStrict . Put.runPut . Put.putWord64be

encodeByteString :: Id a -> ByteString
encodeByteString (Id i) = Base64.encodeUnpadded $ word64ToByteString $ fromIntegral i

encodeText :: Id a -> Text
encodeText = decodeUtf8Lenient . encodeByteString

byteStringToWord64 :: ByteString -> Either String Word64
byteStringToWord64 bs
  | BS.length bs == 8 = Right $ Get.runGet Get.getWord64be $ BS.fromStrict bs
  | otherwise = Left "Invalid encoded table ID"

decodeByteString :: ByteString -> Either String (Id a)
decodeByteString bs = do
  w64 <- Base64.decodeUnpadded bs >>= byteStringToWord64
  pure . Id . fromIntegral $ w64

decodeText :: Text -> Either String (Id a)
decodeText = decodeByteString . encodeUtf8

instance ParsableParam (Id a) where
  parseParam = first (HttpError badRequest400) . decodeText

new :: (MonadIO m) => m (Id a)
new = do
  ts <- liftIO $ systemMilliseconds <$> getSystemTime
  r <- Random.uniformWord32 Random.globalStdGen
  pure . Id $ fromIntegral @Word64 $ (fromIntegral ts `shiftL` 20) .|. fromIntegral (r .&. 0x000f_ffff)

systemMilliseconds :: SystemTime -> Int64
systemMilliseconds st =
  systemSeconds st * 1000
    + fromIntegral (systemNanoseconds st) `div` 1_000_000

utcTime :: Id a -> UTCTime
utcTime (Id i) =
  systemToUTCTime $
    MkSystemTime
      { systemSeconds = seconds,
        systemNanoseconds = fromIntegral ms * 1_000_000
      }
  where
    (seconds, ms) = fromIntegral (i `shiftR` 20) `quotRem` 1000

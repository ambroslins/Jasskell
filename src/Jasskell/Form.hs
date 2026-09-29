module Jasskell.Form
  ( Form,
    fromByteString,
    Parser,
    runParser,
    parseByteString,
    field,
    unique,
    optional,
    values,
    int,
    text,
    renderErrors,
  )
where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Network.HTTP.Types qualified as HTTP

type Form = HTTP.Query

fromByteString :: ByteString -> Form
fromByteString = HTTP.parseQuery

newtype Parser a = Parser (Form -> Either [(ByteString, String)] a)
  deriving (Functor)

instance Applicative Parser where
  pure = Parser . const . Right
  Parser pf <*> Parser px = Parser $ \form ->
    case (pf form, px form) of
      (Right f, Right x) -> Right (f x)
      (Left e1, Left e2) -> Left (e1 <> e2)
      (Left e, _) -> Left e
      (_, Left e) -> Left e

runParser :: Parser a -> Form -> Either [(ByteString, String)] a
runParser = coerce

parseByteString :: Parser a -> ByteString -> Either [(ByteString, String)] a
parseByteString parser = runParser parser . fromByteString

annotate :: ByteString -> Either String a -> Either [(ByteString, String)] a
annotate key = first $ \e -> [(key, e)]

field :: ByteString -> (ByteString -> Either String a) -> Parser a
field key parse =
  Parser $
    annotate key
      . maybe (Left "missing") parse
      . join
      . lookup key

unique :: ByteString -> (ByteString -> Either String a) -> Parser a
unique key parse = Parser $ \form -> annotate key $
  case [v | (k, v) <- form, k == key] of
    [] -> Left "missing"
    [Just x] -> parse x
    _ -> Left "duplicate"

optional :: ByteString -> (ByteString -> Either String a) -> Parser (Maybe a)
optional key parse =
  Parser $
    annotate key
      . maybe (Right Nothing) (fmap Just . parse)
      . join
      . lookup key

values :: ByteString -> (ByteString -> Either String a) -> Parser [a]
values key parse = Parser $ \form ->
  sequence [annotate key (parse v) | (k, Just v) <- form, k == key]

int :: ByteString -> Either String Int
int bs = case BS.readInt bs of
  Just (i, rest) | BS.null rest -> Right i
  _ -> Left "invalid int"

text :: ByteString -> Either String Text
text = first (const "invalid UTF-8") . decodeUtf8'

renderErrors :: [(ByteString, String)] -> Builder
renderErrors = foldMap $ \(key, msg) ->
  Builder.byteString key <> ": " <> Builder.stringUtf8 msg <> "\n"

{-# LANGUAGE TemplateHaskell #-}

module Jasskell.Static (Asset (path, sha256Base64), handlers, style, htmx) where

import Crypto.Hash qualified
import Crypto.Hash.Algorithms (SHA256)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64URL
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LazyBS
import Data.FileEmbed (embedFileRelative)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai (rawPathInfo)
import Web.Twain qualified as Twain
import Web.Twain.Types qualified as Twain

data Asset = Asset
  { content :: !ByteString,
    pathBS :: !ByteString,
    contentType :: !ContentType,
    path :: !Text,
    sha256Base64 :: !Text
  }

handlers :: Twain.Middleware
handlers =
  handleAsset style
    . handleAsset htmx

handleAsset :: Asset -> Twain.Middleware
handleAsset asset =
  Twain.get
    (matchRawPath asset.pathBS)
    $ Twain.send
    $ Twain.raw
      Twain.status200
      [ (Twain.hCacheControl, "public, max-age=31536000, immutable"),
        (Twain.hContentType, asset.contentType.header)
      ]
    $ LazyBS.fromStrict asset.content

style :: Asset
style = makeAsset "style" css $(embedFileRelative "static/style.css")

htmx :: Asset
htmx = makeAsset "htmx" js $(embedFileRelative "static/htmx-4.0.0-beta2.min.js")

makeAsset :: ByteString -> ContentType -> ByteString -> Asset
makeAsset name contentType content =
  Asset {content, path, pathBS, contentType, sha256Base64 = decodeUtf8 hash}
  where
    pathBS =
      "/static/"
        <> BS.intercalate "." [name, BS.take 8 hash, contentType.extension]
    path = decodeUtf8 pathBS
    hash =
      Base64URL.encodeUnpadded . convert $
        Crypto.Hash.hash @ByteString @SHA256 content

matchRawPath :: ByteString -> Twain.PathPattern
matchRawPath path = Twain.MatchPath $ \req ->
  if path == rawPathInfo req then Just [] else Nothing

data ContentType = ContentType
  { extension :: !ByteString,
    header :: !ByteString
  }

css :: ContentType
css = ContentType {extension = "css", header = "text/css; charset=utf-8"}

js :: ContentType
js = ContentType {extension = "js", header = "application/javascript; charset=utf-8"}

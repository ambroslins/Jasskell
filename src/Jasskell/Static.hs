{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Jasskell.Static
  ( Asset (path, sha256Base64),
    handlers,
    style,
    script,
  )
where

import Control.Exception (throwIO)
import Control.Monad (guard)
import Crypto.Hash qualified
import Crypto.Hash.Algorithms (SHA256)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64URL
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed (embedFileRelative)
import Data.Streaming.Zlib qualified as Zlib
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai (rawPathInfo)
import System.IO.Unsafe (unsafePerformIO)
import Web.Twain qualified as Twain
import Web.Twain.Types qualified as Twain

data Asset = Asset
  { content :: LazyByteString,
    contentGzip :: ~LazyByteString,
    pathBS :: ByteString,
    contentType :: ContentType,
    path :: Text,
    sha256Base64 :: Text
  }

handlers :: Twain.Middleware
handlers =
  handleAsset style
    . handleAsset script

handleAsset :: Asset -> Twain.Middleware
handleAsset asset =
  Twain.get (matchRawPath asset.pathBS) $
    do
      headers <- Twain.headers
      let acceptGzip = maybe False hasGzip $ lookup Twain.hAcceptEncoding headers
          (content, encoding) =
            if acceptGzip
              then (asset.contentGzip, [(Twain.hContentEncoding, "gzip")])
              else (asset.content, [])
      Twain.send $
        Twain.raw
          Twain.status200
          ( (Twain.hCacheControl, "public, max-age=31536000, immutable")
              : (Twain.hContentType, asset.contentType.header)
              : encoding
          )
          content

hasGzip :: ByteString -> Bool
hasGzip = any isGzip . BS.split ','
  where
    isGzip alg = case BS.split ';' alg of
      ["gzip"] -> True
      ["gzip", readQuality -> Just q] -> q > 0.0
      _ -> False

readQuality :: ByteString -> Maybe Double
readQuality bs = do
  num <- BS.stripPrefix "q=" $ BS.strip bs
  (i, rest) <- BS.readInt $ BS.strip num
  if BS.null rest
    then pure $ fromIntegral i
    else do
      (frac, trailing) <- BS.stripPrefix "." rest >>= BS.readInt
      guard $ BS.null trailing
      let !base = 10.0 ^^ BS.length trailing
      pure $ fromIntegral i + fromIntegral frac / base

style :: Asset
style = makeAsset "style" css [$(embedFileRelative "static/style.css")]

script :: Asset
script =
  makeAsset
    "script"
    js
    [ $(embedFileRelative "static/htmx-4.0.0-beta6.min.js"),
      $(embedFileRelative "static/hx-ws-4.0.0-beta6.min.js")
    ]

makeAsset :: ByteString -> ContentType -> [ByteString] -> Asset
makeAsset name contentType chunks =
  Asset
    { content,
      contentGzip = gzipDeflate content,
      path = decodeUtf8 pathBS,
      pathBS,
      contentType,
      sha256Base64 = decodeUtf8 hash
    }
  where
    content = LBS.fromChunks chunks
    pathBS =
      "/static/"
        <> BS.intercalate "." [name, BS.take 8 hash, contentType.extension]
    hash =
      Base64URL.encodeUnpadded . convert $
        Crypto.Hash.hashlazy @SHA256 content

matchRawPath :: ByteString -> Twain.PathPattern
matchRawPath path = Twain.MatchPath $ \req ->
  if path == rawPathInfo req then Just [] else Nothing

data ContentType = ContentType
  { extension :: ByteString,
    header :: ByteString
  }

css :: ContentType
css = ContentType {extension = "css", header = "text/css; charset=utf-8"}

js :: ContentType
js = ContentType {extension = "js", header = "application/javascript; charset=utf-8"}

gzipDeflate :: LazyByteString -> LazyByteString
gzipDeflate content = unsafePerformIO $ do
  deflate <- Zlib.initDeflate 7 $ Zlib.WindowBits 31
  let go chunks dlist = case chunks of
        [] -> finalize <$> pop dlist (Zlib.finishDeflate deflate)
        c : cs -> Zlib.feedDeflate deflate c >>= pop dlist >>= go cs
  LBS.fromChunks <$> go (LBS.toChunks content) id
  where
    pop dlist !popper =
      popper >>= \case
        Zlib.PRDone -> pure dlist
        Zlib.PRNext bs -> pop (dlist . (bs :)) popper
        Zlib.PRError e -> throwIO e
    finalize dlist = dlist []
{-# NOINLINE gzipDeflate #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Jasskell.Static
  ( Asset (path, sha256Base64),
    handlers,
    style,
    script,
  )
where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.GZip qualified as GZip
import Control.Monad (guard)
import Crypto.Hash qualified
import Crypto.Hash.Algorithms (SHA256)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64URL
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.FileEmbed (embedFileRelative)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai

data Asset = Asset
  { content :: LazyByteString,
    contentGzip :: ~LazyByteString,
    contentBrotli :: ~LazyByteString,
    pathBS :: ByteString,
    contentType :: ContentType,
    path :: Text,
    sha256Base64 :: Text
  }

handlers :: Wai.Middleware
handlers =
  handleAsset style
    . handleAsset script

data EncodingWeights = EncodingWeights {brotli, gzip :: Float}
  deriving (Show)

handleAsset :: Asset -> Wai.Middleware
handleAsset asset next request respond
  | Wai.rawPathInfo request == asset.pathBS && Wai.requestMethod request == HTTP.methodGet =
      respond $ Wai.responseLBS HTTP.status200 responseHeaders content
  | otherwise = next request respond
  where
    requestHeaders = Wai.requestHeaders request
    weights = parseAcceptEncoding $ lookup HTTP.hAcceptEncoding requestHeaders
    (content, encoding)
      | weights.gzip > weights.brotli = (asset.contentGzip, Just "gzip")
      | weights.brotli > 0.0 = (asset.contentBrotli, Just "br")
      | otherwise = (asset.content, Nothing)
    responseHeaders =
      (HTTP.hCacheControl, "public, max-age=31536000, immutable")
        : (HTTP.hContentLength, showInt64BS $ LBS.length content)
        : (HTTP.hContentType, asset.contentType.header)
        : (HTTP.hVary, CI.original HTTP.hAcceptEncoding)
        : case encoding of
          Nothing -> []
          Just alg -> [(HTTP.hContentEncoding, alg)]
    showInt64BS = LBS.toStrict . Builder.toLazyByteString . Builder.int64Dec

parseAcceptEncoding :: Maybe ByteString -> EncodingWeights
parseAcceptEncoding = maybe none (foldl' go none . BS.split ',')
  where
    none = EncodingWeights {gzip = 0.0, brotli = 0.0}
    go weights encoding = case parseEncodingType $ BS.strip encoding of
      Just (alg, q)
        | alg == "br" -> weights {brotli = q}
        | alg == "gzip" -> weights {gzip = q}
      _ -> weights
    parseEncodingType encoding = case BS.split ';' encoding of
      [alg] -> Just (alg, 1.0)
      [alg, readQuality -> Just q] -> Just (alg, q)
      _ -> Nothing
    readQuality :: ByteString -> Maybe Float
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
style =
  makeAsset
    "style"
    css
    [ $(embedFileRelative "static/pico-2.1.1.green.min.css"),
      $(embedFileRelative "static/custom.css")
    ]

script :: Asset
script =
  makeAsset
    "script"
    js
    [ $(embedFileRelative "static/htmx-4.0.0.min.js"),
      $(embedFileRelative "static/hx-ws-4.0.0.min.js")
    ]

makeAsset :: ByteString -> ContentType -> [ByteString] -> Asset
makeAsset name contentType chunks =
  Asset
    { content,
      contentGzip = GZip.compressWith gzipParams content,
      contentBrotli = Brotli.compressWith brotliParams content,
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
    gzipParams =
      GZip.defaultCompressParams
        { GZip.compressLevel = GZip.compressionLevel 9,
          GZip.compressMemoryLevel = GZip.maxMemoryLevel
        }
    brotliParams =
      Brotli.defaultCompressParams
        { Brotli.compressMode = Brotli.CompressionModeText
        }

data ContentType = ContentType
  { extension :: ByteString,
    header :: ByteString
  }

css :: ContentType
css = ContentType {extension = "css", header = "text/css; charset=utf-8"}

js :: ContentType
js = ContentType {extension = "js", header = "application/javascript; charset=utf-8"}

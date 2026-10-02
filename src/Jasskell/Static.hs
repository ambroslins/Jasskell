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
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.FileEmbed (embedFileRelative)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai

data Encoded = Encoded
  { body :: LazyByteString,
    headers :: HTTP.ResponseHeaders
  }

data Asset = Asset
  { content :: Encoded,
    contentGzip :: Encoded,
    contentBrotli :: Encoded,
    pathBS :: ByteString,
    path :: Text,
    sha256Base64 :: Text
  }

handlers :: Wai.Middleware
handlers next request respond =
  case List.find (\a -> a.pathBS == rawPath) assets of
    Just asset
      | method == HTTP.methodGet || method == HTTP.methodHead ->
          let content
                | weights.gzip > weights.brotli = asset.contentGzip
                | weights.brotli > 0.0 = asset.contentBrotli
                | otherwise = asset.content
           in respond $ Wai.responseLBS HTTP.status200 content.headers content.body
      | otherwise ->
          respond $ Wai.responseLBS HTTP.status405 [(HTTP.hAllow, "GET, HEAD")] mempty
    Nothing -> next request respond
  where
    method = Wai.requestMethod request
    rawPath = Wai.rawPathInfo request
    requestHeaders = Wai.requestHeaders request
    weights = parseAcceptEncoding $ lookup HTTP.hAcceptEncoding requestHeaders
    assets = [script, style]

style, script :: Asset
style =
  makeAsset
    "style"
    css
    [ $(embedFileRelative "static/pico-2.1.1.green.min.css"),
      $(embedFileRelative "static/custom.css")
    ]
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
    { content = encode Nothing content,
      contentGzip =
        encode (Just "gzip") $ GZip.compressWith gzipParams content,
      contentBrotli =
        encode (Just "br") $ Brotli.compressWith brotliParams content,
      path = decodeUtf8 pathBS,
      pathBS,
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
    encode encoding body =
      Encoded
        { body,
          headers =
            (HTTP.hContentLength, BS.pack . show $ LBS.length body)
              : (HTTP.hContentType, contentType.header)
              : (HTTP.hCacheControl, "public, max-age=31536000, immutable")
              : (HTTP.hVary, CI.original HTTP.hAcceptEncoding)
              : foldMap (\alg -> [(HTTP.hContentEncoding, alg)]) encoding
        }
    gzipParams =
      GZip.defaultCompressParams
        { GZip.compressLevel = GZip.compressionLevel 9,
          GZip.compressMemoryLevel = GZip.maxMemoryLevel
        }
    brotliParams =
      Brotli.defaultCompressParams
        { Brotli.compressMode = Brotli.CompressionModeText,
          Brotli.compressLevel = Brotli.CompressionLevel11,
          Brotli.compressSizeHint = fromIntegral (LBS.length content)
        }

data ContentType = ContentType
  { extension :: ByteString,
    header :: ByteString
  }

css, js :: ContentType
css = ContentType {extension = "css", header = "text/css; charset=utf-8"}
js = ContentType {extension = "js", header = "application/javascript; charset=utf-8"}

data EncodingWeights = EncodingWeights {brotli, gzip :: Float}
  deriving (Show)

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
          digits <- BS.stripPrefix "." rest
          (frac, trailing) <- BS.readInt digits
          guard $ BS.null trailing
          let !base = 10.0 ^^ BS.length digits
          pure $ fromIntegral i + fromIntegral frac / base

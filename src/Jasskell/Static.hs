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
import Data.FileEmbed (embedFileRelative)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai (rawPathInfo)
import Web.Twain qualified as Twain
import Web.Twain.Types qualified as Twain

data Asset = Asset
  { content :: LazyByteString,
    contentGzip :: ~LazyByteString,
    contentBrotli :: ~LazyByteString,
    pathBS :: ByteString,
    contentType :: ContentType,
    path :: Text,
    sha256Base64 :: Text
  }

handlers :: Twain.Middleware
handlers =
  handleAsset style
    . handleAsset script

data EncodingWeights = EncodingWeights {brotli, gzip :: Float}
  deriving (Show)

handleAsset :: Asset -> Twain.Middleware
handleAsset asset =
  Twain.get (matchRawPath asset.pathBS) $
    do
      headers <- Twain.headers
      let weights = parseAcceptEncoding $ lookup Twain.hAcceptEncoding headers
          (content, encoding)
            | weights.brotli > weights.gzip || weights.brotli == 1.0 = (asset.contentBrotli, Just "br")
            | weights.gzip > 0.0 = (asset.contentGzip, Just "gzip")
            | otherwise = (asset.content, Nothing)
          responseHeaders =
            (Twain.hCacheControl, "public, max-age=31536000, immutable")
              : (Twain.hContentType, asset.contentType.header)
              : case encoding of
                Nothing -> []
                Just alg -> [(Twain.hContentEncoding, alg)]
      Twain.send $ Twain.raw Twain.status200 responseHeaders content

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
    [ $(embedFileRelative "static/htmx-4.0.0-beta6.min.js"),
      $(embedFileRelative "static/hx-ws-4.0.0-beta6.min.js")
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

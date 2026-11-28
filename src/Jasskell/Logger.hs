module Jasskell.Logger
  ( Logger,
    withStderrLogger,
    Level (..),
    Pair,
    (=:),
    withContext,
    requestLogger,
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception
  ( ExceptionWithContext (..),
    SomeException (..),
    bracket,
    catchNoPropagate,
    rethrowIO,
  )
import Control.Exception.Annotation
  ( SomeExceptionAnnotation (..),
    displayExceptionAnnotation,
  )
import Control.Exception.Context (getAllExceptionAnnotations)
import Control.Monad (when)
import Data.Aeson (ToJSON (toEncoding), fromEncoding)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.System (systemToUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Records (HasField (..))
import Jasskell.Clock (MonotonicClock, SystemClock, SystemTime)
import Jasskell.Clock qualified as Clock
import Network.HTTP.Types (Status (..), hContentLength)
import Network.Wai qualified as Wai
import System.Log.FastLogger
  ( LogStr,
    LogType' (..),
    defaultBufSize,
    newFastLogger1,
    toLogStr,
  )
import Prelude hiding (log)

newtype Logger = Logger (Level -> Text -> [Pair] -> IO ())

data Level = Debug | Info | Warning | Error
  deriving (Eq, Show, Ord)

data Pair = Pair {key :: Text, value :: Builder}
  deriving (Show)

infix 6 =:

(=:) :: (ToJSON a) => Text -> a -> Pair
key =: value = Pair key (fromEncoding $ toEncoding value)

instance HasField "debug" Logger (Text -> [Pair] -> IO ()) where
  getField (Logger log) = log Debug

instance HasField "info" Logger (Text -> [Pair] -> IO ()) where
  getField (Logger log) = log Info

instance HasField "warning" Logger (Text -> [Pair] -> IO ()) where
  getField (Logger log) = log Warning

instance HasField "error" Logger (Text -> [Pair] -> IO ()) where
  getField (Logger log) = log Error

withContext :: Logger -> [Pair] -> Logger
withContext (Logger log) ctx = Logger (\level msg pairs -> log level msg (ctx ++ pairs))

showLevel :: Level -> LogStr
showLevel = \case
  Debug -> "DEBUG"
  Info -> "INFO"
  Warning -> "WARNING"
  Error -> "ERROR"

fmtMessage :: SystemTime -> Level -> ThreadId -> Text -> [Pair] -> LogStr
fmtMessage !time !level !threadId !msg pairs =
  "time="
    <> toLogStr (iso8601Show $ systemToUTCTime time)
    <> " level="
    <> showLevel level
    <> " thread_id="
    <> toLogStr (dropWhile (not . Char.isDigit) $ show threadId)
    <> " msg="
    <> toLogStr (fromEncoding $ toEncoding msg)
    <> foldMap fmtPair pairs
    <> "\n"
  where
    fmtPair Pair {key, value} = " " <> toLogStr key <> "=" <> toLogStr value

withStderrLogger :: SystemClock -> Level -> (Logger -> IO a) -> IO a
withStderrLogger clock minLevel action =
  bracket (newFastLogger1 $ LogStderr defaultBufSize) snd $
    \(logger, _cleanup) ->
      action $ Logger $ \level msg pairs -> when (level >= minLevel) $ do
        time <- Clock.getSystemTime clock
        threadId <- myThreadId
        logger $ fmtMessage time level threadId msg pairs

requestLogger :: MonotonicClock -> Logger -> Wai.Middleware
requestLogger clock (Logger logger) app req respond = do
  start <- Clock.getMonotonicTime clock
  let logResponse result = do
        end <- Clock.getMonotonicTime clock
        let !duration = Clock.elapsed start end
            (!status, !size, rest) = case result of
              Left (ExceptionWithContext ctx (SomeException e)) ->
                (500, Nothing, ["exception" =: show e, "context" =: showContext ctx])
              Right response ->
                let (Status st _) = Wai.responseStatus response
                    sz = do
                      h <- lookup hContentLength $ Wai.responseHeaders response
                      (!s, _) <- BS.readInt h
                      pure s
                 in (st, sz, [])
            !level
              | status >= 500 = Error
              | status >= 400 = Warning
              | otherwise = Info
        logger level "request" $
          "method" =: decodeUtf8Lenient (Wai.requestMethod req)
            : "path" =: decodeUtf8Lenient (Wai.rawPathInfo req)
            : "query" =: decodeUtf8Lenient (Wai.rawQueryString req)
            : "status" =: status
            : "size" =: size
            : "duration" =: duration
            : rest

      respondWithLog response = do
        received <- respond response
        logResponse $ Right response
        pure received

  app req respondWithLog
    `catchNoPropagate` (\e -> logResponse (Left e) >> rethrowIO e)
  where
    showContext =
      map (\(SomeExceptionAnnotation e) -> displayExceptionAnnotation e)
        . getAllExceptionAnnotations

module Jasskell.Logger
  ( Logger,
    withStderrLogger,
    MonadLogger (..),
    Level (..),
    Pair,
    (=:),
    logDebug,
    logInfo,
    logWarning,
    logError,
    requestLogger,
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (bracket)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson (ToJSON (toEncoding), fromEncoding)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.Fixed (Milli)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
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

newtype Logger = Logger (UTCTime -> Level -> Text -> [Pair] -> IO ())

data Level = Debug | Info | Warning | Error
  deriving (Eq, Show, Ord)

data Pair = Pair {key :: !Text, value :: !Builder}
  deriving (Show)

infix 2 =:

(=:) :: (ToJSON a) => Text -> a -> Pair
key =: value = Pair key (fromEncoding $ toEncoding value)

class MonadLogger m where
  askLogger :: m Logger

instance MonadLogger ((->) Logger) where
  askLogger = id

instance (Monad m) => MonadLogger (ReaderT Logger m) where
  askLogger = ask

log :: (MonadIO m, MonadLogger m) => Level -> Text -> [Pair] -> m ()
log level msg pairs = do
  (Logger logger) <- askLogger
  time <- liftIO getCurrentTime
  liftIO $ logger time level msg pairs

logDebug :: (MonadIO m, MonadLogger m) => Text -> [Pair] -> m ()
logDebug = log Debug

logInfo :: (MonadIO m, MonadLogger m) => Text -> [Pair] -> m ()
logInfo = log Info

logWarning :: (MonadIO m, MonadLogger m) => Text -> [Pair] -> m ()
logWarning = log Warning

logError :: (MonadIO m, MonadLogger m) => Text -> [Pair] -> m ()
logError = log Error

showLevel :: Level -> LogStr
showLevel = \case
  Debug -> "DEBUG"
  Info -> "INFO"
  Warning -> "WARNING"
  Error -> "ERROR"

fmtMessage :: UTCTime -> Level -> ThreadId -> Text -> [Pair] -> LogStr
fmtMessage time level threadId msg pairs =
  "time="
    <> toLogStr (fromEncoding $ toEncoding time)
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

withStderrLogger :: Level -> (Logger -> IO a) -> IO a
withStderrLogger minLevel action =
  bracket (newFastLogger1 $ LogStderr defaultBufSize) snd $
    \(logger, _cleanup) ->
      action $ Logger $ \time level msg pairs -> when (level >= minLevel) $ do
        threadId <- myThreadId
        logger $ fmtMessage time level threadId msg pairs

requestLogger :: Logger -> Wai.Middleware
requestLogger (Logger logger) app req respond = do
  start <- getCurrentTime
  app req $ \response -> do
    end <- getCurrentTime
    let !dt = nominalDiffTimeToSeconds (end `diffUTCTime` start)
        (Status status _) = Wai.responseStatus response
        size = do
          h <- lookup hContentLength (Wai.responseHeaders response)
          (s, rest) <- BS.readInt h
          guard (rest == BS.empty) $> s
        level
          | status >= 500 = Error
          | status >= 400 = Warning
          | otherwise = Info
        pairs =
          [ "method" =: decodeUtf8Lenient (Wai.requestMethod req),
            "path" =: decodeUtf8Lenient (Wai.rawPathInfo req),
            "query" =: decodeUtf8Lenient (Wai.rawQueryString req),
            "status" =: status,
            "size" =: size,
            "duration" =: (realToFrac dt :: Milli)
          ]
    logger end level "request" pairs
    respond response

module Jasskell.Server.WebSocket where

import Colog.Message (logInfo)
import Control.Concurrent.Async (concurrently_)
import Data.Aeson.Combinators.Decode (Decoder)
import Data.Aeson.Combinators.Decode qualified as Decoder
import Data.Aeson.Combinators.Encode qualified as Encoder
import Data.ByteString qualified as ByteString
import Jasskell.Server.App (MonadApp, runAppT)
import Jasskell.Server.App qualified as App
import Jasskell.Server.Decoder qualified as Decoder
import Jasskell.Server.Message qualified as Message
import Jasskell.Server.Table (SomeTable (SomeTable))
import Jasskell.Server.Table qualified as Table
import Jasskell.Server.TableID (TableID)
import Jasskell.Server.TableID qualified as TableID
import Network.WebSockets
  ( PendingConnection (pendingRequest),
    ServerApp,
    acceptRequest,
    receiveData,
    rejectRequest,
    requestPath,
    sendTextData,
    withPingThread,
  )

data JoinMessage = JoinMessage
  { username :: Text,
    tableID :: TableID,
    seat :: Int
  }
  deriving (Eq, Show)

joinMessageDecoder :: Decoder JoinMessage
joinMessageDecoder =
  JoinMessage
    <$> Decoder.key "username" Decoder.text
    <*> Decoder.key "tableID" Decoder.auto
    <*> Decoder.key "seat" Decoder.int

server :: App.Env IO -> ServerApp
server env pendingConnection =
  runAppT env (handleConnection pendingConnection)

handleConnection :: (MonadApp m, MonadIO m) => PendingConnection -> m ()
handleConnection pendingConnection = do
  let path = requestPath $ pendingRequest pendingConnection
  case ByteString.stripPrefix "/play/" path >>= TableID.fromByteString of
    Nothing -> do
      liftIO $ rejectRequest pendingConnection $ "Invalid path: " <> path
      logInfo $
        "WebSocket connection rejected because the path: "
          <> show path
          <> " is invalid"
    Just tableID ->
      App.lookupTable tableID >>= \case
        Nothing -> do
          liftIO $ rejectRequest pendingConnection "Table not found"
          logInfo $
            "WebSocket connection rejected becuase the table: "
              <> show tableID
              <> " does not exist"
        Just (SomeTable table) -> do
          connection <- liftIO $ acceptRequest pendingConnection
          logInfo "WebSocket connection accepted"

          (putAction, getMessage) <- Table.join table

          let send = sendTextData connection
              recive = receiveData connection

              sendThread = forever $ do
                message <- getMessage
                send $ Encoder.encode Message.encoder message

              reciveThread = forever $ do
                message <- recive
                case Decoder.eitherDecode Decoder.action message of
                  Left e -> send $ fromString e
                  Right action -> putAction action

          liftIO $
            withPingThread connection 30 pass $
              concurrently_ sendThread reciveThread

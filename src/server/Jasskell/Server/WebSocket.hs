{-# LANGUAGE OverloadedStrings #-}

module Jasskell.Server.WebSocket where

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Text.Lazy (Text)
import Jasskell.Message
import Jasskell.ServerState
import Jasskell.User
import Network.Wai (Middleware)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets

data JoinMessage = Join {tableID :: TableID, username :: String}

instance FromJSON JoinMessage where
  parseJSON =
    withObject "join" $ \o -> Join <$> o .: "tableID" <*> o .: "username"

middleware :: ServerState -> Middleware
middleware =
  websocketsOr defaultConnectionOptions {connectionStrictUnicode = True}
    . server

server :: ServerState -> ServerApp
server state pending = do
  connection <- acceptRequest pending
  msg <- receiveData connection
  print msg
  case decode msg of
    Nothing -> sendClose connection ("invalid join requst" :: Text)
    Just j -> do
      chan <- newChan
      let get = readChan chan
      let put (UpdateGameView g) = sendTextData connection $ encode g
      let user = newUser get put $ username j
      userJoin state (tableID j) user
      forever $ do
        action <- receiveData connection
        maybe (return ()) (writeChan chan) (decode action)

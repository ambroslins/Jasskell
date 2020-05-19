{-# LANGUAGE OverloadedStrings #-}

module Jasskell.Server.WebSocket where

import           Data.Aeson
import           Data.Text.Lazy                 ( Text )
import           Control.Concurrent
import           Control.Monad
import           Network.Wai                    ( Middleware )
import           Network.Wai.Handler.WebSockets ( websocketsOr )
import           Network.WebSockets
import           Jasskell.Message
import           Jasskell.User
import           Jasskell.ServerState


data JoinMessage = Join { tableID :: TableID, username :: String }

instance FromJSON JoinMessage where
    parseJSON =
        withObject "join" $ \o -> Join <$> o .: "tableID" <*> o .: "username"

middleware :: ServerState -> Middleware
middleware =
    websocketsOr defaultConnectionOptions { connectionStrictUnicode = True }
        . server

server :: ServerState -> ServerApp
server state pending = do
    connection <- acceptRequest pending
    msg        <- receiveData connection
    print msg
    case decode msg of
        Nothing -> sendClose connection ("invalid join requst" :: Text)
        Just j  -> do
            chan <- newChan
            let get = readChan chan
            let put (UpdateGameView g) = sendTextData connection $ encode g
            let user = newUser get put $ username j
            userJoin state (tableID j) user
            forever $ do
                action <- receiveData connection
                maybe (return ()) (writeChan chan) (decode action)

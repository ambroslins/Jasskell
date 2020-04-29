{-# LANGUAGE OverloadedStrings #-}

module Jasskell.Server where

import qualified Data.Text.Lazy                as Text
import           Data.Text.Lazy                 ( pack )
import           Jasskell.ServerState
import qualified Jasskell.Server.WebSocket     as WebSocketServer
import           Network.Wai.Middleware.Static
import           Web.Scotty


server :: IO ()
server = do
    state <- emptyServerState
    scotty 9000 $ do
        middleware $ staticPolicy (addBase "static")
        middleware $ WebSocketServer.middleware state
        get "/" $ file "static/index.html"
        get "/newgame" $ do
            gameID <- liftAndCatchIO $ createGame state
            text $ pack (show gameID)
        get "/games" $ do
            games <- liftAndCatchIO $ allGames state
            text $ Text.unlines $ map (pack . show) games

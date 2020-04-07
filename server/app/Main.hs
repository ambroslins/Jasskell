{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector.Sized             as Vector
import           Data.Text
import           Jasskell.Card
import           Jasskell.Game
import           Jasskell.Round
import           Jasskell.User
import           Jasskell.Variant
import           Jasskell.Card.Suit
import           Jasskell.Player
import           System.Random
import qualified Network.WebSockets            as WS
import           GHC.TypeLits
import           Control.Concurrent

main :: IO ()
main = do
    print "listening"
    WS.runServer "127.0.0.1" 9000 $ \penn -> do
        conn <- WS.acceptRequest penn
        print "player connected"
        let
            Just v =
                (Vector.fromList
                    [websocketUser conn "websocket user", cliUser "cli user"]
                ) :: Maybe (Vector.Vector 2 User)
        g <- exampleGame v
        print "game starting"
        g' <- playGame g
        print $ currentUser g'
        return ()



exampleGame :: KnownNat n => Vector.Vector n User -> IO (Game n)
exampleGame us = do
    ps <- fmap newPlayer <$> getStdRandom dealCards
    return Game { users        = us
                , currentUser  = 0
                , currentRound = Playing $ startRound (Trump Bells) 0 ps
                , rounds       = []
                }

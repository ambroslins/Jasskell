{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.Vector.Sized             as Vector
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
    state <- newMVar []
    putStrLn "listening"
    WS.runServer "127.0.0.1" 9000 $ \pen -> do
        c <- WS.acceptRequest pen
        putStrLn "user connected"
        cs <- takeMVar state
        let cs' = c : cs
        case Vector.fromList cs' :: Maybe (Vector.Vector 2 WS.Connection) of
            Just v -> do
                let
                    us = Vector.imap
                        (\i x -> websocketUser x ("websocket user " ++ show i))
                        v
                putMVar state []
                g <- exampleGame us
                _ <- playGame g
                return ()
            Nothing -> putMVar state cs'


exampleGame :: KnownNat n => Vector.Vector n User -> IO (Game n)
exampleGame us = do
    ps <- fmap newPlayer <$> getStdRandom dealCards
    return Game { users        = us
                , currentUser  = 0
                , currentRound = Playing $ startRound (Trump Bells) 0 ps
                , rounds       = []
                }

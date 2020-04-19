{-# LANGUAGE DataKinds #-}

module Main where

import           Data.Aeson
import qualified Data.Vector.Sized             as Vector
import           Jasskell.Card
import           Jasskell.GameState
import           Jasskell.Message
import           Jasskell.Round
import           Jasskell.User
import           Jasskell.Variant
import           Jasskell.Player
import           System.Random
import qualified Network.WebSockets            as WS
import           GHC.TypeLits
import           Control.Concurrent
import           Control.Monad

main :: IO ()
main = do
    mvar <- newEmptyMVar
    let lobby us = do
            u <- takeMVar mvar
            let us' = u : us
            case Vector.fromList us' :: Maybe (Vector.Vector 4 User) of
                Just v -> do
                    g <- exampleGame $ Vector.reverse v
                    _ <- playGame g
                    return ()
                Nothing -> lobby us'
    _ <- forkIO $ lobby []
    putStrLn "listening"
    WS.runServer "127.0.0.1" 9000 $ \pen -> do
        c <- WS.acceptRequest pen
        putStrLn "accepted connection"
        chan <- newChan
        let receive = do
                ma <- decode <$> WS.receiveData c
                maybe (return ()) (writeChan chan) ma
        let u = newUser
                (readChan chan)
                (\(UpdateGameView g) -> WS.sendTextData c $ encode g)
                "Websocket User"
        putMVar mvar u
        forever receive


exampleGame :: KnownNat n => Vector.Vector n User -> IO (GameState n)
exampleGame us = do
    ps <- fmap newPlayer <$> getStdRandom dealCards
    return GameState { users        = us
                     , currentUser  = 0
                     , currentRound = Playing $ startRound (Trump Bells) 0 ps
                     , rounds       = []
                     }

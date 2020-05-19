{-# LANGUAGE DataKinds #-}

module Jasskell.Table
    ( Table
    , newTable
    , joinChan
    )
where

import           Data.Vector.Sized              ( Vector )
import qualified Data.Vector.Sized             as Vector
import           Control.Concurrent
import           Control.Monad
import           System.Random
import           GHC.TypeLits
import           Jasskell.User
import           Jasskell.Game
import           Jasskell.Card
import           Jasskell.Variant
import           Jasskell.Round
import           Jasskell.Player


data Table = Table
    { threadID :: ThreadId
    , joinChan :: Chan User
    }

newTable :: IO Table
newTable = do
    chan   <- newChan
    thread <- forkIO $ gameThread chan -- start game thread
    return Table { threadID = thread, joinChan = chan }

gameThread :: Chan User -> IO ()
gameThread chan = void $ loop []
  where
    loop us = do
        u <- readChan chan
        let us' = u : us
        case Vector.fromList us' :: Maybe (Vector 4 User) of
            Just v -> do
                game <- exampleGame $ Vector.reverse v
                putStrLn "starting Game"
                playGame game
            Nothing -> loop us'

exampleGame :: KnownNat n => Vector.Vector n User -> IO (Game n)
exampleGame us = do
    ps <- fmap newPlayer <$> getStdRandom dealCards
    return Game { users        = us
                , currentUser  = 0
                , currentRound = Playing $ startRound (Trump Bells) 0 ps
                , rounds       = []
                }


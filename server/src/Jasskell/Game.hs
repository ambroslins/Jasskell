module Jasskell.Game
    ( GameID
    , Game
    , newGame
    )
where

import           Control.Concurrent
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )

newtype GameID = GameID UUID deriving (Eq, Ord, Show)


data Game = Game { threadID :: ThreadId
                 , eventChannel :: Chan ()
                 }

newGame :: IO (GameID, Game)
newGame = do
    gid  <- GameID <$> nextRandom
    chan <- newChan
    tid  <- forkIO $ return ()
    return (gid, Game { threadID = tid, eventChannel = chan })

module Jasskell.User
    ( User
    , name
    , getAction
    , putMessage
    , cliUser
    )
where

import           Jasskell.Action
import           Jasskell.Message

data User = User { name :: String
                 , getAction :: IO Action
                 , putMessage :: Message -> IO ()
                 }

cliUser :: String -> User
cliUser n = User { name       = n
                 , getAction  = PlayCard . read <$> getLine
                 , putMessage = \(UpdateGameView gv) -> print gv
                 }

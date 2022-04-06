module Table where

import Action (Action)
import Control.Concurrent.STM (STM, newTBQueue)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad.Except
import Control.Monad.Free (Free)
import Control.Monad.STM (atomically, retry)
import Data.Finite (Finite)
import Data.Maybe (fromMaybe, isJust)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Game (Game)
import Jass (Jass, JassNat)
import Map qualified

newtype Table n = Table {state :: (TVar (TableState n))}

data TableState n
  = Setup (Map (Finite n) User)
  | Playing (TablePlaying n)

data User = User
  { name :: Text,
    sendMessage :: Message -> STM ()
  }

newtype TableSetup n = TableSetup {users :: Map (Finite n) User}

type GameState n = Jass n (Free (Jass n) (Game n))

data TablePlaying n = TablePlaying
  { users :: Vector n User,
    gameState :: GameState n
  }

make :: JassNat n => STM (Table n)
make = Table <$> newTVar (Setup Map.empty)

data Message = Message

joinUser :: forall n. JassNat n => Finite n -> Text -> Table n -> STM (Maybe (Action -> STM (), STM Message))
joinUser i name table =
  readTVar table.state >>= \case
    Playing _ -> pure Nothing
    Setup users -> do
      if Map.member i users
        then pure Nothing
        else do
          msgVar <- newTVar Nothing
          let getMsg =
                readTVar msgVar >>= \case
                  Nothing -> retry
                  Just msg -> msg <$ writeTVar msgVar Nothing
              putMsg msg = writeTVar msgVar (Just msg)
              user = User {name = name, sendMessage = putMsg}
          writeTVar table.state $ Setup $ Map.insert i user users
          pure $ Just (undefined, getMsg)

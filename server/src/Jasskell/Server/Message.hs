module Jasskell.Server.Message
  ( Message (..),
    encoder,
  )
where

import Data.Aeson.Combinators.Encode (Encoder)
import Data.Aeson.Combinators.Encode qualified as Encoder
import Jasskell.Server.Encoder (Tagged (..))
import Jasskell.Server.Encoder qualified as Encoder
import Jasskell.Server.Error (Error)
import Jasskell.Server.PlayerView (PlayerView)
import Jasskell.Server.PlayerView qualified as PlayerView

data Message n
  = UpdatePlayerView (PlayerView n)
  | Error Error
  deriving (Show)

encoder :: KnownNat n => Encoder (Message n)
encoder = Encoder.tagged $ \case
  UpdatePlayerView view -> Tagged "player-view" PlayerView.encoder view
  Error e -> Tagged "error" Encoder.text (show e)
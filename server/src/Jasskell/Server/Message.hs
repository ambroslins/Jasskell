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
import Jasskell.Server.GuestView (GuestView)
import Jasskell.Server.GuestView qualified as GuestView
import Jasskell.Server.PlayerView (PlayerView)
import Jasskell.Server.PlayerView qualified as PlayerView

data Message n
  = UpdatePlayerView (PlayerView n)
  | UpdateGuestView (GuestView n)
  | Error Error
  deriving (Show)

encoder :: KnownNat n => Encoder (Message n)
encoder = Encoder.tagged $ \case
  UpdatePlayerView view -> Tagged "player-view" PlayerView.encoder view
  UpdateGuestView view -> Tagged "guest-view" GuestView.encoder view
  Error e -> Tagged "error" Encoder.text (show e)
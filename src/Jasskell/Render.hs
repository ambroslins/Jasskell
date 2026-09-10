module Jasskell.Render
  ( fragment,
    page,
    index,
    tableLogin,
    tableConnect,
    waitingView,
    playerView,
    spectatorView,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.Identity (runIdentity)
import Data.ByteString.Builder (Builder)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Vector4 qualified as Vector4
import Jasskell.Card (Rank (..), Suit (..))
import Jasskell.Card qualified as Card
import Jasskell.Component qualified as Component
import Jasskell.GameState (GameView (..))
import Jasskell.Id qualified as Id
import Jasskell.Player (Nickname (..), Player (..))
import Jasskell.Static qualified as Static
import Jasskell.Table
  ( Command (..),
    PlayerView (..),
    SpectatorView (..),
    TableId,
    WaitingView (..),
  )
import Jasskell.Variant (Variant (..))
import Lucid
import Lucid.Base (makeAttributes)
import Lucid.Htmx

fragment :: Html () -> Builder
fragment = runIdentity . execHtmlT

page :: Text -> Html () -> Builder
page titel body = runIdentity . execHtmlT $ do
  html_ [lang_ "en", makeAttributes "data-theme" "light"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "htmx-config", content_ "ws.pauseOnBackground:false"]
      title_ $ toHtml titel
      link_ [rel_ "stylesheet", href_ Static.style.path]
      script_
        [ src_ Static.script.path,
          integrity_ $ "sha256-" <> Static.script.sha256Base64
        ]
        ("" :: String)
    body_ body

index :: Maybe Player -> Html ()
index mplayer = do
  header_ [id_ "top", class_ "container nav"] $ do
    a_ [href_ "#top"] $
      span_
        [style_ "font-size: 2rem; font-weight: 600; letter-spacing: 0.15rem"]
        "Jass"
  main_ [] $ do
    section_ [id_ "hero", class_ "container"] $ do
      div_ $ do
        h1_ "Hero Section"
        a_ [href_ "#play"] $ button_ [class_ "primary"] "Play"
      div_ [id_ "hero-deck"] $ do
        Component.card [style_ "transform: rotate(-30deg);"] $ Card.make Bells Six
        Component.card [style_ "transform: rotate(-15deg);"] $ Card.make Acorns Nine
        Component.card [style_ "transform: rotate(0deg);"] $ Card.make Leaves Under
        Component.card [style_ "transform: rotate(15deg);"] $ Card.make Hearts King
        Component.card [style_ "transform: rotate(30deg);"] $ Card.make Bells Ace
    section_ [id_ "play", class_ "container"] $ do
      div_ [id_ "table-list"] $ do
        h3_ "Table List"
      form_ [method_ "post", action_ "/tables"] $ do
        when (isNothing mplayer) $ do
          label_ $ do
            "Nickname"
            input_ [type_ "text", name_ "nickname"]
        label_ $ do
          "Private"
          input_ [type_ "checkbox", name_ "private"]
        button_ [type_ "submit", class_ "primary"] "Create"

tableLogin :: TableId -> Html ()
tableLogin tableId =
  form_
    [ hxPost_ $ "/tables/" <> Id.encodeText tableId <> "/join",
      hxTarget_ "this",
      hxSwap_ "outerHTML"
    ]
    $ do
      label_ [] $ do
        "Nickname"
        input_ [name_ "nickname"]
      button_ [type_ "submit"] "Submit"

tableConnect :: Player -> TableId -> Html ()
tableConnect player tableId = do
  h2_ $ toHtml $ "Hello: " <> player.nickname.toText
  div_
    [ hxWsConnect_ $ "/tables/" <> Id.encodeText tableId,
      hxTarget_ "#message",
      hxSwap_ "innerHTML"
    ]
    $ div_ [id_ "message"]
    $ p_ "connecting"

waitingView :: WaitingView -> Html ()
waitingView view = do
  Vector4.iforM_ view.seats $ \i m -> case m of
    Nothing -> div_ $ do
      "Empty"
      button_ [hxWsSend_, hxVals_ $ TakeSeat i] "Take"
    Just nickname
      | Just i == view.yourSeat -> div_ "You"
      | otherwise -> div_ $ toHtml nickname.toText
  when (all isJust view.seats) $ button_ [hxWsSend_, hxVals_ StartGame] "Start"

playerView :: PlayerView -> Html ()
playerView view = do
  div_ . toHtml $ show view
  div_ $ case view.game.variant of
    Nothing
      | view.game.currentPlayer == 0 -> forM_ [minBound .. maxBound] $ \s ->
          button_ [hxWsSend_, hxVals_ $ DeclareVariant $ Trump s] $ toHtml $ show s
      | otherwise -> "Waiting for leader to declare variant"
    Just v -> toHtml $ show v
  Vector4.iforM_ view.seats $ \i nickname ->
    article_ $ do
      div_ . toHtml $ if i == 0 then "You" else nickname.toText
      case Vector4.index i view.game.playedCards of
        Nothing -> div_ "-"
        Just c -> div_ . toHtml $ Card.abbreviation c

spectatorView :: SpectatorView -> Html ()
spectatorView = toHtml . show

module Jasskell.Render where

import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Jasskell.Card (Rank (..), Suit (..))
import Jasskell.Card qualified as Card
import Jasskell.Component qualified as Component
import Jasskell.Player (Player)
import Jasskell.Static qualified as Static
import Lucid
import Lucid.Base (makeAttributes)

page :: Text -> Html () -> Html ()
page titel body = do
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
index mplayer = page "Jasskell" $ do
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

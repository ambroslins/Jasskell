module Jasskell.Skeleton where

import Data.Text (Text)
import Jasskell.Static qualified as Static
import Lucid

skeleton :: (Monad m) => Text -> HtmlT m () -> HtmlT m ()
skeleton title body = do
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "htmx-config", content_ "ws.pauseOnBackground:false"]
      title_ $ toHtml title
      link_ [rel_ "stylesheet", href_ Static.style.path]
      script_
        [ src_ Static.htmx.path,
          integrity_ $ "sha256-" <> Static.htmx.sha256Base64
        ]
        ("" :: String)
      script_
        [ src_ Static.htmxWebSockets.path,
          integrity_ $ "sha256-" <> Static.htmxWebSockets.sha256Base64
        ]
        ("" :: String)
    body_ body

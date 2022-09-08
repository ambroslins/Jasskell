module Jasskell.Server.Html.Index where

import Lucid

index :: Html ()
index = html_ $ do
  head_ $ do
    title_ "Jasskell"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "style.css"]
    script_ [src_ "main.js"] ("" :: String)
  body_ $ do
    div_ [id_ "app"] ""
    script_ [src_ "index.js"] ("" :: String)

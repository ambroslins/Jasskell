module Jasskell.Server.Html.Index where

import Lucid

index :: Html ()
index = html_ $ do
  head_ $ do
    title_ "Jasskell"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "style.css"]
  body_ $ do
    h1_ "Hello World"

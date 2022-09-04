module Jasskell.Server.Html.Index where

import Lucid

index :: Html ()
index = html_ $ do
  head_ $ do
    title_ "Jasskell"
  body_ $ do
    h1_ "Hello World"
module Jasskell.Server.Page (routes) where

import Jasskell.Server.Http qualified as Http
import Lucid
import Network.Wai (Response)
import Web.Twain qualified as Twain

routes :: [Http.Route]
routes =
  [ Http.get "/" $ html index
  , Http.get "/play/:tableID" $ html index
  ]

html :: Monad m => HtmlT m a -> m Response
html m = Twain.html <$> renderBST m

index :: Monad m => HtmlT m ()
index = html_ $ do
  makeHead "Jasskell"
  body_ initElm

makeHead :: Monad m => Text -> HtmlT m ()
makeHead title =
  head_ $ do
    title_ $ toHtml title
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/style.css"]
    script_ [src_ "/main.js"] ("" :: String)

initElm :: Monad m => HtmlT m ()
initElm =
  script_ [src_ "/index.js"] ("" :: String)

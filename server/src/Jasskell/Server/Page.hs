module Jasskell.Server.Page
  ( Route,
    server,
  )
where

import Jasskell.Server.Html (HTML)
import Lucid
import Servant (Get, NamedRoutes, Server)
import Servant.API.Generic (type (:-))

type Route = NamedRoutes Routes

newtype Routes mode = Routes
  { home :: mode :- Get '[HTML] (Html ())
  }
  deriving (Generic)

server :: Server Route
server =
  Routes
    { home = pure $
        html_ $ do
          makeHead "Jasskell"
          body_
            initElm
    }

makeHead :: Monad m => Text -> HtmlT m ()
makeHead title =
  head_ $ do
    title_ $ toHtml title
    link_ [rel_ "stylesheet", type_ "text/css", href_ "style.css"]
    script_ [src_ "main.js"] ("" :: String)

initElm :: Monad m => HtmlT m ()
initElm = do
  div_ [id_ "app"] ""
  script_ "const app = Elm.Main.init();"

module Jasskell.Server.Page
  ( Route,
    server,
  )
where

import Jasskell.Server.App (AppT)
import Jasskell.Server.Html (HTML)
import Lucid
import Servant (Get, Handler, NamedRoutes, ServerT)
import Servant.API.Generic (type (:-))

type Route = NamedRoutes Routes

newtype Routes mode = Routes
  { home :: mode :- Get '[HTML] (Html ())
  }
  deriving (Generic)

server :: ServerT Route (AppT Handler)
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
initElm =
  script_ [src_ "index.js"] ("" :: String)

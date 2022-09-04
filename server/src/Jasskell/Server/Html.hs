module Jasskell.Server.Html
  ( Route,
    server,
  )
where

import Jasskell.Server.Html.Index (index)
import Lucid (Html, ToHtml (toHtml), renderBS)
import Network.HTTP.Media ((//), (/:))
import Servant (Server)
import Servant.API (Accept (contentTypes), Get, MimeRender (mimeRender))

data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ =
    "text" // "html" /: ("charset", "utf-8")
      :| ["text" // "html"]

instance ToHtml a => MimeRender HTML a where
  mimeRender _ = renderBS . toHtml

type Route = Get '[HTML] (Html ())

server :: Server Route
server = pure index
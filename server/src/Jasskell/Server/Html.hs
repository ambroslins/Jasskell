module Jasskell.Server.Html
  ( HTML,
  )
where

import Lucid (ToHtml (toHtml), renderBS)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (contentTypes), MimeRender (mimeRender))

data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ =
    "text" // "html" /: ("charset", "utf-8")
      :| ["text" // "html"]

instance ToHtml a => MimeRender HTML a where
  mimeRender _ = renderBS . toHtml

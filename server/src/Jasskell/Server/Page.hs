module Jasskell.Server.Page (routes) where

import Jasskell.Server.App (MonadApp)
import Jasskell.Server.App qualified as App
import Jasskell.Server.Http qualified as Http
import Lucid
import Network.Wai (Response)
import Web.Twain qualified as Twain

routes :: [Http.Route]
routes =
  [ Http.get "/" $ html index,
    Http.get "/play" $ html play,
    Http.post "/play" createTable,
    Http.get "/play/:tableID" $ html index
  ]

html :: Monad m => HtmlT m a -> m Response
html m = Twain.html <$> renderBST m

makePage :: Monad m => Text -> HtmlT m () -> HtmlT m ()
makePage title body = do
  head_ $ do
    title_ $ toHtml title
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/style.css"]
  body_ $
    div_
      [class_ "w-screen h-screen flex justify-center items-center"]
      body

index :: Monad m => HtmlT m ()
index =
  makePage "Jasskell" $
    a_
      [ href_ "/play",
        class_ "text-4xl rounded-xl p-1",
        class_ "border-2 border-lime-400 hover:bg-lime-400"
      ]
      "Play"

play :: (MonadApp m, MonadIO m) => HtmlT m ()
play = do
  tableIDs <- lift App.activeTables
  form_ [action_ "/play", method_ "post"] $
    button_
      [ class_ "text-4xl rounded-xl p-1 m-3",
        class_ "border-2 border-lime-400 hover:bg-lime-400"
      ]
      "Create Table"
  table_ $ do
    tr_ (th_ "Table ID")
    forM_ tableIDs $ \tableID ->
      tr_ $
        td_ $
          span_ $ do
            toHtml @Text $ show tableID
            a_ "Join"

createTable :: (MonadApp m, MonadIO m) => m Response
createTable = do
  tableID <- App.createTable
  pure $ Twain.redirect302 $ "/play/" <> show tableID
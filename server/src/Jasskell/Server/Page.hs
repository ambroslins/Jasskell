module Jasskell.Server.Page (routes) where

import Data.Aeson (Value (String), encode)
import Data.Text qualified as Text
import Jasskell.Server.App (MonadApp)
import Jasskell.Server.App qualified as App
import Jasskell.Server.Http (ResponderT)
import Jasskell.Server.Http qualified as Http
import Jasskell.Server.TableID qualified as TableID
import Lucid
import Network.Wai (Response)
import UnliftIO (MonadUnliftIO)
import Web.Twain qualified as Twain

routes :: (MonadApp m, MonadUnliftIO m) => [Http.Route m]
routes =
  [ Http.get "/" (pure $ html index),
    Http.get "/play" (html <$> play),
    Http.post "/play" createTable,
    Http.get "/play/:tableID" (html <$> playActive)
  ]

html :: Html () -> Response
html = Twain.html . renderBS

makePage :: Monad m => Text -> HtmlT m () -> HtmlT m ()
makePage title body = do
  head_ $ do
    title_ (toHtml title)
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/style.css"]
  body_ [class_ "w-screen h-screen flex justify-center items-center"] body

index :: Monad m => HtmlT m ()
index =
  makePage "Jasskell" $
    a_
      [ href_ "/play",
        class_ "text-4xl rounded-xl p-1",
        class_ "border-2 border-lime-400 hover:bg-lime-400"
      ]
      "Play"

play :: (MonadApp m, MonadUnliftIO m) => ResponderT m (Html ())
play = do
  tableIDs <- lift App.activeTables
  pure $
    makePage "Play Jasskell" $ do
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
                toHtml (TableID.toText tableID)
                a_ [href_ ("/play/" <> show tableID)] "Join"

playActive :: MonadIO m => ResponderT m (Html ())
playActive = do
  tableID <- Http.param "tableID"
  pure $
    makePage "Play Jasskell" $ do
      elmElement "Play" (String (TableID.toText tableID))
      script_ [src_ "/index.js"] Text.empty

createTable :: (MonadApp m, MonadUnliftIO m) => ResponderT m Response
createTable = do
  tableID <- lift App.createTable
  pure $ Twain.redirect302 $ "/play/" <> show tableID

elmElement :: Monad m => Text -> Value -> HtmlT m ()
elmElement name flags = do
  div_ [id_ name] "Loading"
  script_ [src_ "/main.js"] Text.empty
  script_
    ( "var app = Elm." <> name <> ".init({node:" <> node
        <> ", flags: "
        <> decodeUtf8 (encode flags)
        <> "});"
    )
  where
    node = "document.getElementById('" <> name <> "')"
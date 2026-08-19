module Jasskell.Server (application) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Jasskell.App (AppT, Env (..), hoistAppT, runAppT)
import Jasskell.Card (Rank (..), Suit (..))
import Jasskell.Card qualified as Card
import Jasskell.Component qualified as Component
import Jasskell.Id qualified as Id
import Jasskell.Logger
import Jasskell.Player (Player)
import Jasskell.Player qualified as Player
import Jasskell.Skeleton (skeleton)
import Jasskell.Static qualified as Static
import Jasskell.Table (Connection (..), JoinError (..), TableId, TableManager)
import Jasskell.Table qualified as Table
import Lucid
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_, hxWsConnect_)
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import UnliftIO.Async qualified as Async
import UnliftIO.STM (atomically)
import Web.Cookie (parseCookies)
import Web.Twain qualified as Twain

application :: Env -> TableManager -> Wai.Application
application env tableManager =
  websocketsOr
    WS.defaultConnectionOptions
    (websocketApp env tableManager)
    $ requestLogger env.logger
    $ foldr ($) (Twain.notFound $ Twain.send $ Twain.html "Not found...")
    $ Static.handlers
      : routes env tableManager

websocketApp :: Env -> TableManager -> WS.ServerApp
websocketApp env tm pending = rejectOnError . runExceptT . runAppT env $ do
  let request = WS.pendingRequest pending
  tableId <- case parseTableId (WS.requestPath request) of
    Nothing -> throwError $ WS.defaultRejectRequest {WS.rejectCode = 404}
    Just t -> pure t
  sessionCookie <- case parseSessionCookie (WS.requestHeaders request) of
    Nothing -> throwError $ WS.defaultRejectRequest {WS.rejectCode = 401}
    Just sc -> pure sc
  player <-
    Player.fromSessionCookie sessionCookie >>= \case
      Left err -> do
        logError "invalid session cookie" ["error" =: show err]
        throwError $ WS.defaultRejectRequest {WS.rejectCode = 401}
      Right s -> pure s
  hoistAppT ExceptT $
    Table.join player tableId tm $
      runExceptT . \case
        Left TableNotFound -> throwError $ WS.defaultRejectRequest {WS.rejectCode = 404}
        Right Connection {receive} -> lift $ do
          connection <- liftIO $ WS.acceptRequest pending
          logDebug "accepted websocket request" []
          let sendLoop = do
                msg <- liftIO $ WS.receiveData @Text connection
                logDebug "got message" ["message" =: msg]
                sendLoop
              receiveLoop = do
                msg <- atomically receive
                liftIO $ WS.sendTextData connection $ Text.show msg
                receiveLoop
          Async.race_ sendLoop receiveLoop
  where
    rejectOnError m =
      m >>= \case
        Left rr -> runAppT env $ do
          logError "reject websocket request" ["code" =: WS.rejectCode rr]
          liftIO $ WS.rejectRequestWith pending rr
        Right () -> pure ()
    parseTableId path = do
      t <- BS.stripPrefix "/tables/" path
      either (const Nothing) Just $ Id.decodeByteString t
    parseSessionCookie headers = do
      cookies <- lookup Twain.hCookie headers
      lookup Player.sessionCookieName $ parseCookies cookies

routes :: Env -> TableManager -> [Twain.Middleware]
routes env tm =
  [ Twain.get "/" $ runAppT env $ getRoot tm,
    Twain.post "/tables" $ runAppT env postTables,
    Twain.get "/tables/:table-id" $ runAppT env getTable,
    Twain.post "/tables/:table-id/join" $ runAppT env postTableJoin
  ]

getRoot :: TableManager -> AppT Twain.ResponderM ()
getRoot _tm = do
  mplayer <- getPlayerSession
  lift $ Twain.send $ Twain.html $ renderBS $ skeleton "Jasskell" $ do
    header_ [id_ "top", class_ "container nav"] $ do
      a_ [href_ "#top"] $
        span_
          [style_ "font-size: 2rem; font-weight: 600; letter-spacing: 0.15rem"]
          "Jass"
    main_ [] $ do
      section_ [id_ "hero", class_ "container"] $ do
        div_ $ do
          h1_ "Hero Section"
          a_ [href_ "#play"] $ button_ [class_ "primary"] "Play"
        div_ [id_ "hero-deck"] $ do
          Component.card [style_ "transform: rotate(-30deg);"] $ Card.make Bells Six
          Component.card [style_ "transform: rotate(-15deg);"] $ Card.make Acorns Nine
          Component.card [style_ "transform: rotate(0deg);"] $ Card.make Leaves Under
          Component.card [style_ "transform: rotate(15deg);"] $ Card.make Hearts King
          Component.card [style_ "transform: rotate(30deg);"] $ Card.make Bells Ace
      section_ [id_ "play", class_ "container"] $ do
        div_ [id_ "table-list"] $ do
          h3_ "Table List"
        form_ [method_ "post", action_ "/tables"] $ do
          when (isNothing mplayer) $ do
            label_ $ do
              "Nickname"
              input_ [type_ "text", name_ "nickname"]
          label_ $ do
            "Private"
            input_ [type_ "checkbox", name_ "private"]
          button_ [type_ "submit", class_ "primary"] "Create"

postTables :: AppT Twain.ResponderM ()
postTables = do
  _private <- lift $ Twain.paramMaybe @Bool "private"
  (playerId, addCookie) <-
    getPlayerSession >>= \case
      Just p -> pure (p.id, id)
      Nothing -> do
        nickname <- lift $ Twain.param "nickname"
        (p, setCookie) <- Player.newSession nickname
        pure (p.id, Twain.withCookie' setCookie)
  tableId <- Table.create playerId
  lift $ Twain.send $ addCookie . Twain.redirect303 $ "/tables/" <> Id.encodeText tableId

getTable :: AppT Twain.ResponderM ()
getTable = do
  tableId <- lift $ Twain.param @TableId "table-id"
  mplayer <- getPlayerSession
  lift . Twain.send . Twain.html . renderBS . skeleton "Jass Table" $ do
    h1_ $ toHtml $ "Found table: " <> Id.encodeText tableId
    main_ $
      case mplayer of
        Nothing -> form_
          [ hxPost_ $ "/tables/" <> Id.encodeText tableId <> "/join",
            hxTarget_ "main"
          ]
          $ do
            label_ [] $ do
              "Nickname"
              input_ [name_ "nickname"]
            button_ [type_ "submit"] "Submit"
        Just player -> viewTable player tableId

viewTable :: Player -> TableId -> Html ()
viewTable player tableId = do
  h2_ $ toHtml $ "Hello: " <> player.nickname.toText
  div_ [hxWsConnect_ $ "/tables/" <> Id.encodeText tableId, hxSwap_ "innerHTML", hxTarget_ "this"] $
    p_ "connecting"

postTableJoin :: AppT Twain.ResponderM ()
postTableJoin = do
  tableId <- lift $ Twain.param @TableId "table-id"
  nickname <- lift $ Twain.param "nickname"
  mplayer <- getPlayerSession
  case mplayer of
    Just _ -> error "TODO: change nickname"
    Nothing -> do
      (player, setCookie) <- Player.newSession nickname
      lift . Twain.send . Twain.withCookie' setCookie . Twain.html . renderBS $
        viewTable player tableId

getPlayerSession :: AppT Twain.ResponderM (Maybe Player)
getPlayerSession =
  lift (Twain.cookieParamMaybe Player.sessionCookieName) >>= \case
    Nothing -> pure Nothing
    Just sessionCookie ->
      Player.fromSessionCookie sessionCookie >>= \case
        Left err -> do
          logWarning "invalid session cookie" ["error" =: show err]
          lift . Twain.send . Twain.status Twain.status401 . Twain.expireCookie Player.sessionCookieName $
            Twain.text "Unauthorized"
        Right player -> pure $ Just player

module Jasskell.Render
  ( fragment,
    page,
    index,
    tableLogin,
    tableConnect,
    waitingView,
    playerView,
    spectatorView,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.Identity (runIdentity)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Vector.Unboxed qualified as VU
import Data.Vector4 qualified as Vector4
import Jasskell.Card (Card, Rank (..), Suit (..))
import Jasskell.Card qualified as Card
import Jasskell.Card.Seq qualified as CardSeq
import Jasskell.Card.Set qualified as CardSet
import Jasskell.GameState (CardStatus (Playable), GameView (..), HandCard (..))
import Jasskell.Id qualified as Id
import Jasskell.Player (Nickname (..), Player (..))
import Jasskell.Static qualified as Static
import Jasskell.Table
  ( Command (..),
    PlayerView (..),
    SpectatorView (..),
    TableId,
    WaitingView (..),
  )
import Jasskell.Variant (Variant (..))
import Lucid
import Lucid.Base (makeAttributes, makeElement)
import Lucid.Htmx
import System.Random qualified as Random

fragment :: Html () -> Builder
fragment = runIdentity . execHtmlT

page :: Text -> Html () -> Builder
page titel body = runIdentity . execHtmlT $ do
  html_ [lang_ "en", makeAttributes "data-theme" "light"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "htmx-config", content_ "ws.pauseOnBackground:false"]
      title_ $ toHtml titel
      link_ [rel_ "stylesheet", href_ Static.style.path]
      script_
        [ src_ Static.script.path,
          integrity_ $ "sha256-" <> Static.script.sha256Base64
        ]
        ("" :: String)
    body_ $ do
      svgSymbols
      body

index :: Random.StdGen -> Maybe Player -> Html ()
index gen mplayer = do
  header_ [id_ "hero", class_ "container grid"] $ do
    div_ $ do
      hgroup_ $ do
        h1_ "Jass"
        p_ "Something useful"
      p_ "More about rules and stuff"
      a_ [href_ "#play", role_ "button"] "Play"

    div_ [id_ "hero-deck"] . VU.foldMap (card []) . VU.take 4 . fst $
      CardSet.shuffle gen

  main_ [id_ "play", class_ "container grid"] $ do
    section_ $ do
      h2_ "New table"
      form_ [method_ "post", action_ "/tables"] $ do
        when (isNothing mplayer) $ label_ $ do
          "Nickname"
          input_
            [ type_ "text",
              name_ "nickname",
              required_ "",
              minlength_ "2",
              maxlength_ "20",
              autocomplete_ "nickname",
              makeAttributes "aria-describedby" "nickname-help"
            ]
          small_ [id_ "nickname-help"] "Shown to other players. No account needed."
        label_ $ do
          "Private"
          input_ [type_ "checkbox", name_ "private"]
        button_ [type_ "submit"] "Create"

tableLogin :: TableId -> Html ()
tableLogin tableId =
  form_
    [ hxPost_ $ "/tables/" <> Id.encodeText tableId <> "/join",
      hxTarget_ "this",
      hxSwap_ "outerHTML"
    ]
    $ do
      label_ [] $ do
        "Nickname"
        input_ [name_ "nickname"]
      button_ [type_ "submit"] "Submit"

tableConnect :: Player -> TableId -> Html ()
tableConnect player tableId = do
  h2_ $ toHtml $ "Hello: " <> player.nickname.toText
  div_
    [ hxWsConnect_ $ "/tables/" <> Id.encodeText tableId,
      hxTarget_ "#message",
      hxSwap_ "innerHTML"
    ]
    $ div_ [id_ "message"]
    $ p_ "connecting"

waitingView :: WaitingView -> Html ()
waitingView view = do
  Vector4.iforM_ view.seats $ \i m -> case m of
    Nothing -> div_ $ do
      "Empty"
      button_ [hxWsSend_, hxVals_ $ TakeSeat i] "Take"
    Just nickname
      | Just i == view.yourSeat -> div_ "You"
      | otherwise -> div_ $ toHtml nickname.toText
  when (all isJust view.seats) $ button_ [hxWsSend_, hxVals_ StartGame] "Start"

playerView :: PlayerView -> Html ()
playerView view = do
  div_ . toHtml $ show view
  div_ $ case view.game.variant of
    Nothing
      | view.game.currentPlayer == 0 -> forM_ [minBound .. maxBound] $ \s ->
          button_ [hxWsSend_, hxVals_ $ DeclareVariant $ Trump s] $ toHtml $ show s
      | otherwise -> "Waiting for leader to declare variant"
    Just v -> toHtml $ show v
  Vector4.iforM_ view.seats $ \i nickname ->
    article_ $ do
      div_ . toHtml $ if i == 0 then "You" else nickname.toText
      case CardSeq.index view.game.playedCards (fromEnum $ i - view.game.trickLeader) of
        Nothing -> div_ "-"
        Just c -> div_ . toHtml $ Card.abbreviation c
  forM_ view.game.hand $ \handCard ->
    button_
      [ hxWsSend_,
        hxVals_ $ PlayCard handCard.card,
        if handCard.status == Playable then mempty else disabled_ "true"
      ]
      $ toHtml
      $ Card.abbreviation handCard.card

spectatorView :: SpectatorView -> Html ()
spectatorView = toHtml . show

card :: [Attributes] -> Card -> Html ()
card as c = div_ (class_ "card" : as) $ do
  span_ [class_ "rank top"] corner
  span_ [class_ "suit"] suit
  span_ [class_ "rank bottom"] corner
  where
    corner = rank <> suit
    suit = case Card.suit c of
      Bells -> bell []
      Acorns -> acorn []
      Leaves -> leaf []
      Hearts -> heart []
    rank = case Card.rank c of
      Six -> "6"
      Seven -> "7"
      Eight -> "8"
      Nine -> "9"
      Ten -> "10"
      Under -> "U"
      Over -> "O"
      King -> "K"
      Ace -> "A"

bell, acorn, heart, leaf :: [Attributes] -> Html ()
bell = symbolIcon "#bell"
acorn = symbolIcon "#acorn"
heart = symbolIcon "#heart"
leaf = symbolIcon "#leaf"

symbolIcon :: Text -> [Attributes] -> Html ()
symbolIcon ref as =
  svg_ (class_ "icon" : as) $
    makeElement "use" [href_ ref] mempty

svgSymbols :: Html ()
svgSymbols =
  svg_ [width_ "0", height_ "0", style_ "position: absolute;", makeAttributes "aria-hidden" "true"] $ do
    symbol
      "bell"
      """
      <path d="M96,192a32,32,0,0,0,64,0" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
      <path d="M56,104a72,72,0,0,1,144,0c0,35.82,8.3,64.6,14.9,76A8,8,0,0,1,208,192H48a8,8,0,0,1-6.88-12C47.71,168.6,56,139.81,56,104Z" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
      """
    symbol
      "acorn"
      """
      <path d="M216,112v16c0,53-88,88-88,112,0-24-88-59-88-112V112" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
      <path d="M80,56h96a48,48,0,0,1,48,48v0a8,8,0,0,1-8,8H40a8,8,0,0,1-8-8v0A48,48,0,0,1,80,56Z" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
      <path d="M128,56V48a32,32,0,0,1,32-32" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
      """
    symbol
      "heart"
      """
      <path d="M128,224l89.36-90.64a50,50,0,1,0-70.72-70.72L128,80,109.36,62.64a50,50,0,0,0-70.72,70.72Z" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
      """
    symbol
      "leaf"
      """
      <path d="M63.81,192.19c-47.89-79.81,16-159.62,151.64-151.64C223.43,176.23,143.62,240.08,63.81,192.19Z" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
      <line x1="160" y1="96" x2="40" y2="216" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
      """
  where
    symbol :: Text -> ByteString -> Html ()
    symbol name =
      makeElement "symbol" [id_ name, makeAttributes "viewBox" "0 0 256 256"]
        . toHtmlRaw

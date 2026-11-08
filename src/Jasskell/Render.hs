module Jasskell.Render
  ( fragment,
    page,
    index,
    gameList,
    gameLogin,
    gameConnect,
    waitingView,
    playerView,
    spectatorView,
  )
where

import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Vec qualified as Vec
import Data.Vector (Vector)
import Data.Vector.Unboxed qualified as VU
import GHC.TypeLits (KnownNat)
import Jasskell.Card (Card, Rank (..), Suit (..))
import Jasskell.Card qualified as Card
import Jasskell.Card.Set qualified as CardSet
import Jasskell.Dealer qualified as Dealer
import Jasskell.Game
  ( Action (..),
    GameId,
    PublicGame (..),
  )
import Jasskell.Id qualified as Id
import Jasskell.Player (Nickname (..), Player (..))
import Jasskell.Round (CardStatus (Playable))
import Jasskell.Round qualified as Round
import Jasskell.Static qualified as Static
import Jasskell.Variant (Variant (..))
import Jasskell.View qualified as View
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
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
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
      div_ [id_ "toasts"] mempty

index :: Random.StdGen -> Maybe Player -> Html ()
index gen mplayer = do
  header_ [id_ "hero", class_ "container grid"] $ do
    div_ $ do
      hgroup_ $ do
        h1_ "Jass"
        p_ "Something useful"
      p_ "More about rules and stuff"
      a_ [href_ "#play", role_ "button"] "Play"

    div_ [id_ "hero-deck"] . VU.foldMap (cardDiv []) . VU.take 4 . fst $
      Dealer.shuffle CardSet.deck gen

  main_ [id_ "play", class_ "container grid"] $ do
    section_ $ do
      h2_ "Open games"
      div_
        [ id_ "game-list",
          hxGet_ "/games",
          hxTrigger_ "revealed once",
          hxSwap_ "outerHTML",
          makeAttributes "aria-busy" "true",
          style_ "height: var(--max-height);"
        ]
        mempty

    section_ $ do
      h2_ "New game"
      form_ [method_ "post", action_ "/games"] $ do
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

gameList :: Vector PublicGame -> Html ()
gameList games =
  ul_
    [ id_
        "game-list",
      hxGet_ "/games",
      hxTrigger_ "every 5s [document.visibilityState === 'visible']",
      hxSwap_ "outerMorph"
    ]
    $ forM_ games
    $ \game ->
      let gameId = Id.encodeText game.id
       in li_ [id_ gameId] $ do
            strong_ $ toHtml game.creatorName.toText
            small_ $ toHtml $ show game.seatsTaken <> "/4 players"
            a_
              [ href_ $ "/games/" <> gameId,
                role_ "button",
                class_ $ if game.seatsTaken < 4 then "primary" else "secondary"
              ]
              "Join"

gameLogin :: GameId -> Html ()
gameLogin gameId =
  form_
    [ hxPost_ $ "/games/" <> Id.encodeText gameId <> "/join",
      hxTarget_ "this",
      hxSwap_ "outerHTML"
    ]
    $ do
      label_ [] $ do
        "Nickname"
        input_ [name_ "nickname"]
      button_ [type_ "submit"] "Submit"

gameConnect :: Player -> GameId -> Html ()
gameConnect player gameId = do
  h2_ $ toHtml $ "Hello: " <> player.nickname.toText
  div_
    [ hxWsConnect_ $ "/games/" <> Id.encodeText gameId,
      hxTarget_ "#message",
      hxSwap_ "innerMorph"
    ]
    $ div_ [id_ "message"]
    $ p_ "connecting"

waitingView :: forall n. (KnownNat n) => View.Waiting n -> Html ()
waitingView view = do
  Vec.iforM_ view.seats $ \i m -> case m of
    Nothing -> div_ $ do
      "Empty"
      unless view.seated $ button_ [hxWsSend_, hxVals_ $ TakeSeat i] "Take"
    Just player
      | i == 0 -> div_ "You"
      | otherwise -> div_ $ toHtml player.nickname.toText
  when (Vec.all isJust view.seats) $ button_ [hxWsSend_, hxVals_ $ StartGame @n] "Start"

playerView :: forall n. (KnownNat n) => View.Playing n -> Html ()
playerView view = do
  div_ . toHtml $ show view
  div_ $ case view.variant of
    Nothing
      | view.currentPlayer == 0 -> forM_ [minBound .. maxBound] $ \s ->
          button_ [hxWsSend_, hxVals_ $ DeclareVariant @n $ Trump s] $ toHtml $ show s
      | otherwise -> "Waiting for leader to declare variant"
    Just v -> toHtml $ show v
  div_ [class_ "trick"] $ do
    div_ [class_ "top"] $ do
      div_ . toHtml $ (Vec.index view.players 2).nickname.toText
      case Vec.index view.playedCards 2 of
        Nothing -> div_ "-"
        Just c -> cardDiv [] c
    div_ [class_ "left"] $ do
      div_ . toHtml $ (Vec.index view.players 1).nickname.toText
      case Vec.index view.playedCards 1 of
        Nothing -> div_ "-"
        Just c -> cardDiv [] c
    div_ [class_ "right"] $ do
      div_ . toHtml $ (Vec.index view.players 3).nickname.toText
      case Vec.index view.playedCards 3 of
        Nothing -> div_ "-"
        Just c -> cardDiv [] c
    div_ [class_ "bottom"] $ do
      case Vec.index view.playedCards 0 of
        Nothing -> div_ "-"
        Just c -> cardDiv [] c
  div_ [class_ "hand"] $
    Round.foldHand view.hand $ \c status ->
      cardButton
        [ hxWsSend_,
          hxVals_ $ PlayCard @n c,
          if status == Playable then mempty else disabled_ "true"
        ]
        c

spectatorView :: (KnownNat n) => View.Spectating n -> Html ()
spectatorView = toHtml . show

cardDiv :: [Attributes] -> Card -> Html ()
cardDiv as = div_ (class_ "card" : as) . cardContent

cardButton :: [Attributes] -> Card -> Html ()
cardButton as = button_ (class_ "card" : as) . cardContent

cardContent :: Card -> Html ()
cardContent c = do
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

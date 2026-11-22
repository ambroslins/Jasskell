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
    closedView,
    supersededView,
    rejectionMessage,
  )
where

import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Fin qualified as Fin
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
    Rejection (..),
  )
import Jasskell.Id qualified as Id
import Jasskell.Player (Nickname (..), Player (..))
import Jasskell.Round (CardStatus (Playable), PlayError (..), Violation (..))
import Jasskell.Round qualified as Round
import Jasskell.Static qualified as Static
import Jasskell.Variant (Direction (..), Variant (..))
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
        p_ "Schieber for four, right in your browser."
      p_ "Create a game, share the link with three friends and play. No account, no download: just pick a nickname."
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
        when (isNothing mplayer) nicknameField
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
      nicknameField
      button_ [type_ "submit"] "Join game"

-- | Matches the rules of 'Jasskell.Player.parseNickname' where HTML allows it.
nicknameField :: Html ()
nicknameField = label_ $ do
  "Nickname"
  input_
    [ type_ "text",
      name_ "nickname",
      required_ "",
      minlength_ "2",
      maxlength_ "20",
      pattern_ "[\\p{L}\\p{N}]+([\\-_][\\p{L}\\p{N}]+)*",
      autocomplete_ "nickname",
      makeAttributes "aria-describedby" "nickname-help"
    ]
  small_ [id_ "nickname-help"] "Letters, digits, - and _. Shown to other players. No account needed."

gameConnect :: Player -> GameId -> Html ()
gameConnect player gameId = do
  nav_ [class_ "container"] $ do
    ul_ $ li_ $ a_ [href_ "/"] $ strong_ "Jass"
    ul_ $ li_ $ playerName player
  div_
    [ hxWsConnect_ $ "/games/" <> Id.encodeText gameId,
      hxTarget_ "#message",
      hxSwap_ "innerMorph"
    ]
    $ div_ [id_ "message"]
    $ p_ [class_ "container", makeAttributes "aria-busy" "true"] "Connecting…"

closedView :: Html ()
closedView =
  notice
    "The game was closed"
    "Something went wrong on the server. Reload to rejoin where you left off."
    "Reload"

supersededView :: Html ()
supersededView =
  notice
    "Opened somewhere else"
    "You opened this game in another tab or on another device."
    "Play here"

-- | A full-width message with a button that reloads the page (and with it the
-- websocket connection).
notice :: Html () -> Html () -> Html () -> Html ()
notice title body action =
  article_ [class_ "container notice"] $ do
    h3_ title
    p_ body
    a_ [href_ "", role_ "button"] action

rejectionMessage :: Rejection n -> Text
rejectionMessage = \case
  SeatTaken -> "That seat is already taken."
  AlreadySeated -> "You already have a seat."
  NotAPlayer -> "Only seated players can do that."
  VariantAlreadyDeclared -> "The variant has already been declared."
  GameAlreadyStarted -> "The game has already started."
  GameNotStarted -> "The game hasn't started yet."
  NotYourTurn -> "It's not your turn."
  PlayError e -> case e of
    CannotPlayNow -> "You can't play a card right now."
    CardNotInHand -> "That card is not in your hand."
    ForbiddenCard v -> violationMessage v

violationMessage :: Violation -> Text
violationMessage = \case
  DoesNotFollow s -> "You must follow suit (" <> suitName s <> ")."
  WouldUndertrump c -> "You may not undertrump the " <> cardName c <> "."

suitName :: Suit -> Text
suitName = \case
  Bells -> "Bells"
  Acorns -> "Acorns"
  Leaves -> "Leaves"
  Hearts -> "Hearts"

rankName :: Rank -> Text
rankName = \case
  Six -> "6"
  Seven -> "7"
  Eight -> "8"
  Nine -> "9"
  Ten -> "10"
  Under -> "Under"
  Over -> "Over"
  King -> "King"
  Ace -> "Ace"

cardName :: Card -> Text
cardName c = rankName (Card.rank c) <> " of " <> suitName (Card.suit c)

waitingView :: forall n. (KnownNat n) => View.Waiting n -> Html ()
waitingView view = do
  Vec.ifor_ view.seats $ \i m -> case m of
    Nothing -> div_ $ do
      "Empty"
      unless view.seated $ button_ [hxWsSend_, hxVals_ $ TakeSeat i] "Take"
    Just player
      | i == 0 -> div_ "You"
      | otherwise -> div_ $ toHtml player.nickname.toText
  when (Vec.all isJust view.seats) $ button_ [hxWsSend_, hxVals_ $ StartGame @n] "Start"

playerView :: forall n. (KnownNat n) => View.Playing n -> Html ()
playerView view = do
  div_ [class_ "container"] $ do
    case view.round.variant of
      Nothing -> variantChoice view
      Just v -> p_ [class_ "variant-status"] $ variantBadge v
    section_ [class_ "table"] $ do
      Fin.for_ $ \i -> div_ [class_ "seat"] $ do
        unless (i == 0) $ div_ [] $ do
          playerName $ Vec.index view.players i
          when (Just i == view.round.stoeck) $ div_ "Stöck"
        case Vec.index view.round.playedCards i of
          Nothing -> cardPlaceholder []
          Just c -> cardDiv [class_ "winning" | Just i == view.round.trickWinner] c

    div_ [class_ "hand"] $
      Round.foldHand view.round.hand $ \c status ->
        cardButton
          [ hxWsSend_,
            hxVals_ $ PlayCard @n c,
            if status == Playable then mempty else disabled_ "true"
          ]
          c

-- | Shown while no variant is declared. The current player picks one, everyone
-- else sees who they are waiting for. The hand stays visible below, so the
-- chooser can decide based on their cards.
variantChoice :: forall n. (KnownNat n) => View.Playing n -> Html ()
variantChoice view
  | view.round.currentPlayer == 0 =
      article_ [class_ "variant-choice"] $ do
        header_ $ do
          strong_ "Choose the variant"
          when view.round.shoved $
            small_ "Your partner passed the choice to you (geschoben)."
        variantGroup "Trump" $ map Trump [minBound .. maxBound]
        variantGroup "No trump" $
          [Direction TopDown, Direction BottomUp, Slalom TopDown, Slalom BottomUp]
  | otherwise =
      p_ [class_ "variant-status", makeAttributes "aria-busy" "true"] $ do
        playerName $ Vec.index view.players view.round.currentPlayer
        " is choosing the variant…"
  where
    variantGroup :: Html () -> [Variant] -> Html ()
    variantGroup title vs = div_ [class_ "variant-group", role_ "group", makeAttributes "aria-label" "variants"] $ do
      small_ title
      div_ [class_ "variant-options"] $ forM_ vs variantButton
    variantButton v =
      button_ [hxWsSend_, hxVals_ $ DeclareVariant @n v, class_ "outline secondary"] $ do
        span_ [class_ "variant-icon"] $ variantIcon v
        span_ [class_ "variant-name"] $ toHtml $ variantName v
        small_ $ toHtml $ variantHint v

-- | The active variant. For Slalom it shows the direction of the current trick,
-- which alternates (see 'Jasskell.Variant.next').
variantBadge :: Variant -> Html ()
variantBadge v = span_ [class_ "variant-badge"] $ do
  span_ [class_ "variant-icon"] $ variantIcon v
  toHtml $ case v of
    Trump s -> suitName s <> " trump"
    Direction d -> directionName d
    Slalom d -> "Slalom · " <> directionName d

variantIcon :: Variant -> Html ()
variantIcon = \case
  Trump s -> suitIcon s
  Direction d -> arrow d
  Slalom d -> arrow d <> arrow (flipDirection d)
  where
    arrow :: Direction -> Html ()
    arrow d = span_ [makeAttributes "aria-hidden" "true"] $ case d of
      TopDown -> "↓"
      BottomUp -> "↑"
    flipDirection = \case
      TopDown -> BottomUp
      BottomUp -> TopDown

variantName :: Variant -> Text
variantName = \case
  Trump s -> suitName s
  Direction d -> directionName d
  Slalom _ -> "Slalom"

variantHint :: Variant -> Text
variantHint = \case
  Trump _ -> "trump"
  Direction TopDown -> "high cards win"
  Direction BottomUp -> "low cards win"
  Slalom d -> "starts " <> directionName d

directionName :: Direction -> Text
directionName = \case
  TopDown -> "Obenabe"
  BottomUp -> "Undenufe"

suitIcon :: Suit -> Html ()
suitIcon s = span_ [class_ cls] $ icon [makeAttributes "aria-hidden" "true"]
  where
    (icon, cls) = case s of
      Bells -> (bell, "bells")
      Acorns -> (acorn, "acorns")
      Leaves -> (leaf, "leaves")
      Hearts -> (heart, "hearts")

playerName :: Player -> Html ()
playerName p = span_ [class_ "player-name"] $ do
  strong_ . toHtml $ p.nickname.toText
  small_ $ "#" <> toHtml (show $ Id.toInt p.id `mod` 10000)

spectatorView :: (KnownNat n) => View.Spectating n -> Html ()
spectatorView = toHtml . show

cardDiv :: [Attributes] -> Card -> Html ()
cardDiv = makeCard div_

cardButton :: [Attributes] -> Card -> Html ()
cardButton = makeCard button_

makeCard :: ([Attributes] -> Html () -> Html ()) -> [Attributes] -> Card -> Html ()
makeCard elem_ as c = elem_ (class_ cls : as) $ do
  span_ [class_ "rank top"] corner
  span_ [class_ "suit"] suit
  span_ [class_ "rank bottom"] corner
  where
    corner = rank <> suit
    (suit, cls) = case Card.suit c of
      Bells -> (bell [], "card bells")
      Acorns -> (acorn [], "card acorns")
      Leaves -> (leaf [], "card leaves")
      Hearts -> (heart [], "card hearts")
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

cardPlaceholder :: [Attributes] -> Html ()
cardPlaceholder as = div_ (class_ "card placeholder" : as) mempty

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

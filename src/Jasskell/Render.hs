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
import Data.Text qualified as Text
import Data.Vec qualified as Vec
import Data.Vector (Vector)
import Data.Vector.Unboxed qualified as VU
import GHC.TypeLits (KnownNat)
import Jasskell.Card (Card, Rank (..), Suit (..))
import Jasskell.Card qualified as Card
import Jasskell.Card.Set qualified as CardSet
import Jasskell.Dealer qualified as Dealer
import Jasskell.Meld (Meld (..))
import Jasskell.Meld qualified as Meld
import Jasskell.Game
  ( Action (..),
    GameId,
    GameInfo (..),
    PublicGame (..),
    Rejection (..),
  )
import Jasskell.Id qualified as Id
import Jasskell.Player (Nickname (..), Player (..))
import Jasskell.Round (CardStatus, PlayError (..), ShoveError (..), Violation (..))
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
  html_ [lang_ "en"] $ do
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
    div_ [class_ "stack"] $ do
      hgroup_ $ do
        h1_ "Jass"
        p_ "Schieber for four, right in your browser."
      p_ "Create a game, share the link with three friends and play. No account, no download: just pick a nickname."
      a_ [href_ "#play", class_ "button"] "Play"

    div_ [id_ "hero-deck"] . VU.foldMap (cardDiv []) . VU.take 4 . fst $
      Dealer.shuffle CardSet.deck gen

  main_ [id_ "play", class_ "container grid"] $ do
    section_ [class_ "stack"] $ do
      h2_ "Open games"
      div_
        [ id_ "game-list",
          class_ "panel",
          hxGet_ "/games",
          hxTrigger_ "revealed once",
          hxSwap_ "outerHTML",
          makeAttributes "aria-busy" "true"
        ]
        mempty

    section_ [class_ "stack"] $ do
      h2_ "New game"
      form_ [method_ "post", action_ "/games"] $ do
        when (isNothing mplayer) $ nicknameField []
        label_ $ do
          "Private"
          input_ [type_ "checkbox", name_ "private"]
        button_ [type_ "submit"] "Create"

gameList :: Vector PublicGame -> Html ()
gameList games =
  ul_
    [ id_ "game-list",
      class_ "panel",
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
                class_ $ if game.seatsTaken < 4 then "button small" else "button small secondary"
              ]
              "Join"

-- | Shown to visitors without a session who open a game link. On submit the
-- whole @#join@ section is replaced by 'gameConnect'.
gameLogin :: GameId -> GameInfo -> Html ()
gameLogin gameId info = div_ [id_ "join"] $ do
  nav_ [class_ "container"] $
    a_ [href_ "/"] "Jass"
  main_ [class_ "container"] $
    article_ [class_ "join panel stack"] $ do
      hgroup_ $ do
        h2_ . toHtml $ case info.creatorName of
          Just name -> name.toText <> "’s game"
          Nothing -> "Jass game"
        p_ $
          if info.started
            then "This game has already started. You can watch as a spectator."
            else "You're invited to a game of Schieber."
      unless info.started $ seatDots info.seatsTaken
      form_
        [ hxPost_ $ "/games/" <> Id.encodeText gameId <> "/join",
          hxTarget_ "#join",
          hxSwap_ "outerHTML"
        ]
        $ do
          nicknameField [autofocus_]
          button_ [type_ "submit"] $ if info.started then "Watch game" else "Join game"

-- | Four dots, filled for every taken seat.
seatDots :: Int -> Html ()
seatDots taken =
  div_ [class_ "seat-dots"] $ do
    span_ [makeAttributes "aria-hidden" "true"] $
      forM_ [1 .. 4 :: Int] $ \i ->
        span_ [class_ $ if i <= taken then "dot taken" else "dot"] mempty
    small_ . toHtml $ show taken <> " of 4 seats taken"

-- | Matches the rules of 'Jasskell.Player.parseNickname' where HTML allows it.
nicknameField :: [Attributes] -> Html ()
nicknameField as = label_ $ do
  "Nickname"
  input_ $
    as
      <> [ type_ "text",
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
    a_ [href_ "/"] "Jass"
    playerName player
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
  article_ [class_ "container notice panel stack"] $ do
    h3_ title
    p_ body
    a_ [href_ "", class_ "button"] action

rejectionMessage :: Rejection n -> Text
rejectionMessage = \case
  SeatTaken -> "That seat is already taken."
  AlreadySeated -> "You already have a seat."
  NotAPlayer -> "Only seated players can do that."
  VariantAlreadyDeclared -> "The variant has already been declared."
  GameAlreadyStarted -> "The game has already started."
  GameNotStarted -> "The game hasn't started yet."
  NotYourTurn -> "It's not your turn."
  ShoveError e -> case e of
    CannotShoveNow -> "You can only pass the choice before a variant is declared."
    AlreadyShoved -> "The choice has already been passed to your partner."
  PlayError e -> case e of
    CannotPlayNow -> "You can't play a card right now."
    CardNotInHand -> "That card is not in your hand."
    ForbiddenCard v -> violationMessage v
  RoundNotFinished -> "The round isn't finished yet."
  MeldAlreadyAnnounced -> "You already announced your Weis."

-- | Why a card in the hand can't be played; 'Nothing' if it can.
statusMessage :: CardStatus -> Maybe Text
statusMessage = \case
  Round.Playable -> Nothing
  Round.VariantNotChosen -> Just "The variant hasn't been chosen yet."
  Round.NotYourTurn -> Just "It's not your turn."
  Round.Forbidden v -> Just $ violationMessage v

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
-- | The lobby before the game starts. Seats are laid out like the table during
-- play. Once seated, the view is rotated so you sit at the bottom and your
-- partner opposite you; before that, seats are shown with their team.
waitingView view = div_ [class_ "container stack waiting"] $ do
  hgroup_ $ do
    h2_ "Waiting for players"
    p_ $
      if view.seated
        then "The player opposite you is your partner."
        else "Pick a seat. The player opposite you will be your partner."
  section_ [class_ "lobby"] $ do
    Vec.ifor_ view.seats seat
    div_ [class_ "lobby-center"] $
      if
        | emptySeats > 0 ->
            p_ [makeAttributes "aria-busy" "true"] . toHtml $
              "Waiting for " <> show emptySeats <> (if emptySeats == 1 then " more player…" else " more players…")
        | view.seated -> button_ [hxWsSend_, hxVals_ $ StartGame @n] "Start game"
        | otherwise -> p_ [makeAttributes "aria-busy" "true"] "Waiting for the game to start…"
  button_
    [ type_ "button",
      class_ "outline",
      hxOn_
        "click"
        """
        Promise.resolve()
          .then(() => navigator.clipboard.writeText(location.href))
          .then(
            () => toast('Invite link copied.', 'info'),
            () => toast('Could not copy the link. Copy it from the address bar instead.'),
          )
        """
    ]
    "Copy invite link"
  where
    emptySeats = length . filter isNothing $ Vec.toList view.seats
    seat i = \case
      Nothing -> div_ [class_ "lobby-seat panel empty"] $ do
        small_ $ seatLabel i
        if view.seated
          then span_ "Open seat"
          else button_ [hxWsSend_, hxVals_ $ TakeSeat i, class_ "small"] "Sit here"
      Just player -> div_ [class_ $ "lobby-seat panel" <> (if view.seated && i == 0 then " you" else "")] $ do
        small_ $ seatLabel i
        playerName player
    seatLabel i
      | not view.seated = if even (Fin.toInt i) then "Team 1" else "Team 2"
      | i == 0 = "You"
      | even (Fin.toInt i) = "Partner"
      | otherwise = "Opponent"

playerView :: forall n. (KnownNat n) => View.Playing n -> Html ()
playerView view = div_ [class_ "container"] $ do
  -- Exactly one viewport tall (minus the nav), with the hand at the bottom.
  -- Anything added after this section scrolls into view below the hand.
  section_ [class_ "game-main"] $ do
    -- Always rendered with a fixed height, so the table doesn't jump when
    -- the variant gets declared.
    -- One row of uniformly styled badges: variant left, score right.
    div_ [class_ "game-status"] $ do
      case view.round.variant of
        Just v -> variantBadge v
        Nothing
          | choosing -> mempty
          | otherwise ->
              span_ [class_ "status-badge panel", makeAttributes "aria-busy" "true"] $ do
                playerName $ Vec.index view.players view.round.currentPlayer
                if view.round.shoved
                  then " is choosing (geschoben)…"
                  else " is choosing…"
      scoreBadge view
    -- Panels (variant choice, round summary) are stacked on top of the table
    -- in the same grid cell. They can make the cell taller, but never push
    -- the hand down as long as everything fits into the viewport.
    div_ [class_ "table-area"] $ do
      section_ [class_ "table"] $ do
        Fin.for_ $ \i -> div_ [class_ "seat"] $ do
          -- The own tags sit beside the card, so they are not wrapped.
          if i == 0
            then seatTags view i
            else div_ $ do
              playerName $ Vec.index view.players i
              seatTags view i
          case Vec.index view.round.playedCards i of
            -- While the Weis can be skipped, the own card isn't played yet:
            -- show them on top of the empty slot.
            Nothing
              | i == 0 && canSkipMeld -> div_ [class_ "slot"] $ do
                  cardPlaceholder []
                  meldPanel ownMelds
              -- The trick that was just collected stays on the table for a
              -- moment, then fades out (CSS).
              | Just cards <- shownTrick -> div_ [class_ "slot"] $ do
                  cardPlaceholder []
                  cardDiv
                    [class_ $ if i == view.round.leader then "collected winning" else "collected"]
                    (Vec.index cards i)
              | otherwise -> cardPlaceholder []
            Just c -> cardDiv [class_ "winning" | Just i == view.round.trickWinner] c
      when choosing $ variantChoice view
      forM_ view.result $ roundSummary view

    -- The skip switch is included in every card's message. 'PlayCard' with
    -- 'False' leaves the field out of hx-vals, so it can't override it.
    -- While the last trick is still shown, the hand can't be clicked ("hold").
    div_ [class_ $ if isJust shownTrick then "hand hold" else "hand", hxIncludeInherited_ "#skip-meld"] $
      Round.foldHand view.round.hand $ \c status ->
        cardButton
          ( [ hxWsSend_,
              hxVals_ $ PlayCard @n c False,
              -- Unplayable cards say why on hover.
              case statusMessage status of
                Nothing -> mempty
                Just reason -> disabled_ "true" <> makeAttributes "data-tooltip" reason
            ]
              <> [class_ "meld" | canSkipMeld, c `CardSet.member` meldCards]
          )
          c
  where
    choosing = isNothing view.round.variant && view.round.currentPlayer == 0
    -- The previous trick, as long as nobody has played into the current one.
    shownTrick
      | Vec.all isNothing view.round.playedCards = view.round.lastTrick
      | otherwise = Nothing
    -- Same condition as 'Round.skipMeld': first trick, own card not played.
    ownMelds = Vec.index view.round.melds 0
    canSkipMeld =
      not (null ownMelds)
        && isNothing view.round.lastTrick
        && isNothing (Vec.index view.round.playedCards 0)
    meldCards = foldl' (\s m -> CardSet.union s (Meld.cards m)) CardSet.empty ownMelds

-- | What a player has shown at the table: during the first trick the kind of
-- Weis they announced with their card, afterwards the Weis of the team that
-- won them, and Stöck.
seatTags :: View.Playing n -> Fin.Fin n -> Html ()
seatTags view i =
  unless (isNothing announcement && null revealed && not stoeck) $
    ul_ [class_ "seat-tags"] $ do
      forM_ announcement $ li_ . toHtml . announcementLabel
      forM_ revealed $ \m -> li_ $ do
        meldLabel m
        span_ [class_ "muted"] . toHtml . show $ Meld.points m
      when stoeck $ li_ "Stöck"
  where
    announcement = Vec.index view.round.announcments i
    -- During the first trick the view only has the viewer's own Weis, which
    -- are not announced yet.
    revealed
      | isJust view.round.lastTrick = Vec.index view.round.melds i
      | otherwise = []
    stoeck = Just i == view.round.stoeck

-- | Only the kind of a Weis: neither its cards nor its points, which would
-- give away the rank of a quartet.
announcementLabel :: Meld -> Text
announcementLabel = \case
  Sequence _ len -> "Sequence of " <> Text.show len
  Quartet _ -> "Quartet"

-- | The viewer's Weis, shown on top of their empty card slot, and a toggle to
-- not announce them. The toggle is preserved across re-renders, since its
-- state only reaches the server with the player's first card.
meldPanel :: [Meld] -> Html ()
meldPanel melds = div_ [class_ "meld-panel panel"] $ do
  small_ "Your Weis"
  ul_ $ forM_ melds $ \m -> li_ $ do
    meldLabel m
    span_ [class_ "points"] . toHtml . show $ Meld.points m
  label_ [class_ "meld-skip"] $ do
    input_
      [ type_ "checkbox",
        name_ "skipMeld",
        id_ "skip-meld",
        hxPreserve_
      ]
    "Skip"

meldLabel :: Meld -> Html ()
meldLabel = \case
  Sequence high len -> do
    suitIcon (Card.suit high)
    toHtml $ Text.show len <> " to " <> rankName (Card.rank high)
  Quartet rank -> toHtml $ "Four " <> rankName rank <> "s"

-- | Game score of the previous rounds plus the card points of this round.
-- Team 0 is the viewer's team.
scoreBadge :: View.Playing n -> Html ()
scoreBadge view =
  span_ [class_ "status-badge panel score"] $ do
    team "Us" 0
    span_ [class_ "muted", makeAttributes "aria-hidden" "true"] "·"
    team "Them" 1
  where
    team :: Text -> Round.Team -> Html ()
    team name i = span_ [class_ "team"] $ do
      span_ [class_ "muted"] $ toHtml name
      toHtml . show $ Vec.index view.score i
      -- Running card points; the summary shows the round's full result.
      when (isNothing view.result) $
        span_ [class_ "muted", title_ "Card points this round"] $
          toHtml $ "+" <> show (Vec.index view.round.points i)

-- | Panel over the table once all cards are played. Any seated player can
-- deal the next round.
roundSummary :: forall n. (KnownNat n) => View.Playing n -> Round.Result -> Html ()
roundSummary view result =
  article_
    [ class_ "round-summary panel stack",
      role_ "dialog",
      makeAttributes "aria-labelledby" "round-summary-title"
    ]
    $ do
      hgroup_ $ do
        h3_ [id_ "round-summary-title"] "Round finished"
        case result.match of
          Just 0 -> p_ "Match! Your team won every trick."
          Just _ -> p_ "Match for the opponents."
          Nothing -> mempty
      table_ $ do
        thead_ $ tr_ $ do
          td_ mempty
          th_ [scope_ "col"] "Us"
          th_ [scope_ "col"] "Them"
        tbody_ $ do
          row "Cards" $ Vec.index result.cardPoints
          forM_ result.match $ \winner ->
            row "Match" $ \team -> if team == winner then Round.matchBonus else 0
          row "Weis" $ Vec.index result.meldPoints
          row "Stöck" $ Vec.index result.stoeckPoints
        tfoot_ $ do
          row "Round" $ Vec.index roundTotal
          row "Game" $ \team -> Vec.index view.score team + Vec.index roundTotal team
      button_ [hxWsSend_, hxVals_ $ NextRound @n] "Next round"
  where
    roundTotal = Round.total result
    row :: Text -> (Round.Team -> Int) -> Html ()
    row label points = tr_ $ do
      th_ [scope_ "row"] $ toHtml label
      td_ . toHtml . show $ points 0
      td_ . toHtml . show $ points 1

-- | Dialog-like panel over the table for the player who declares the variant.
-- Not a modal @<dialog>@: the hand below must stay visible to decide.
variantChoice :: forall n. (KnownNat n) => View.Playing n -> Html ()
variantChoice view =
  article_
    [ class_ "variant-choice panel stack",
      role_ "dialog",
      makeAttributes "aria-labelledby" "variant-choice-title"
    ]
    $ do
      hgroup_ $ do
        h3_ [id_ "variant-choice-title"] "Choose the variant"
        when view.round.shoved $
          p_ "Your partner passed the choice to you (geschoben)."
      variantGroup "Trump" $ map Trump [minBound .. maxBound]
      variantGroup "No trump" $
        [Direction TopDown, Direction BottomUp, Slalom TopDown, Slalom BottomUp]
      -- Only the original chooser may shove, and only once.
      unless view.round.shoved $
        footer_ [class_ "variant-shove"] $ do
          button_ [hxWsSend_, hxVals_ $ Shove @n, class_ "secondary"] "Schieben"
          small_ "Let your partner choose instead."
  where
    variantGroup :: Text -> [Variant] -> Html ()
    variantGroup title vs = div_ [class_ "variant-group", role_ "group", makeAttributes "aria-label" title] $ do
      small_ $ toHtml title
      div_ [class_ "variant-options"] $ forM_ vs variantButton
    variantButton v =
      button_ [hxWsSend_, hxVals_ $ DeclareVariant @n v] $ do
        span_ [class_ "variant-icon"] $ variantIcon v
        span_ $ toHtml $ variantName v
        small_ $ toHtml $ variantHint v

-- | The active variant. For Slalom it shows the direction of the current trick,
-- which alternates (see 'Jasskell.Variant.next').
variantBadge :: Variant -> Html ()
variantBadge v = span_ [class_ "status-badge panel"] $ do
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

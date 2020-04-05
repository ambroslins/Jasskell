module Main exposing (..)

import Array as Array exposing (Array)
import Browser
import Css exposing (..)
import Html as Unstyled
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode


main =
    Browser.sandbox { init = init, update = update, view = view }


type Suit
    = Bells
    | Hearts
    | Acorns
    | Leaves


encodeSuit : Suit -> Encode.Value
encodeSuit s =
    Encode.string (showSuit s)


decodeSuit : Decode.Decoder Suit
decodeSuit =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Bells" ->
                        Decode.succeed Bells

                    "Hearts" ->
                        Decode.succeed Hearts

                    "Acorns" ->
                        Decode.succeed Acorns

                    "Leaves" ->
                        Decode.succeed Leaves

                    somethingElse ->
                        Decode.fail <| "Unknown suit: " ++ somethingElse
            )


showSuit : Suit -> String
showSuit s =
    case s of
        Bells ->
            "Bells"

        Hearts ->
            "Hearts"

        Acorns ->
            "Acorns"

        Leaves ->
            "Leaves"


type Rank
    = Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Under
    | Over
    | King
    | Ace


showRank : Rank -> String
showRank r =
    case r of
        Six ->
            "Six"

        Seven ->
            "Seven"

        Eight ->
            "Eight"

        Nine ->
            "Nine"

        Ten ->
            "Ten"

        Under ->
            "Under"

        Over ->
            "Over"

        King ->
            "King"

        Ace ->
            "Ace"


encodeRank : Rank -> Encode.Value
encodeRank r =
    Encode.string (showRank r)


decodeRank : Decode.Decoder Rank
decodeRank =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Six" ->
                        Decode.succeed Six

                    "Seven" ->
                        Decode.succeed Seven

                    "Eight" ->
                        Decode.succeed Eight

                    "Nine" ->
                        Decode.succeed Nine

                    "Ten" ->
                        Decode.succeed Ten

                    "Under" ->
                        Decode.succeed Under

                    "Over" ->
                        Decode.succeed Over

                    "King" ->
                        Decode.succeed King

                    "Ace" ->
                        Decode.succeed Ace

                    somethingElse ->
                        Decode.fail <| "Unknown rank: " ++ somethingElse
            )


type alias Card =
    { suit : Suit
    , rank : Rank
    }


showCard : Card -> String
showCard c =
    showSuit c.suit ++ " " ++ showRank c.rank


encodeCard : Card -> Encode.Value
encodeCard c =
    Encode.object [ ( "suit", encodeSuit c.suit ), ( "rank", encodeRank c.rank ) ]


decodeCard : Decode.Decoder Card
decodeCard =
    Decode.map2 Card (Decode.field "suit" decodeSuit) (Decode.field "rank" decodeRank)


type alias Model =
    { hand : Array Card
    , table : Array (Maybe Card)
    }


modelDecoder : Decode.Decoder Model
modelDecoder =
    Decode.map2 Model
        (Decode.field "hand" (Decode.array decodeCard))
        (Decode.field "table" (Decode.array (Decode.nullable decodeCard)))


init : Model
init =
    { hand =
        Array.fromList
            (List.map (\r -> { suit = Bells, rank = r })
                [ Six
                , Seven
                , Eight
                , Nine
                , Ten
                , Under
                , Over
                , King
                , Ace
                ]
            )
    , table = Array.repeat 4 Nothing
    }


type Msg
    = RemoveCard Card


update : Msg -> Model -> Model
update msg model =
    case msg of
        RemoveCard c ->
            { model
                | hand = Array.filter (\x -> x /= c) model.hand
            }


viewCard : Card -> Html Msg
viewCard card =
    div
        [ css
            [ width (rem 10)
            , height (rem 15)
            , border (px 5)
            , borderStyle solid
            , borderColor (rgb 255 0 0)
            ]
        ]
        [ text (showCard card) ]


viewHandCard : Int -> Int -> Card -> Html Msg
viewHandCard size i card =
    let
        angle =
            0.1
                * (toFloat i
                    - (toFloat (size - 1) / 2)
                  )
    in
    div
        [ onClick (RemoveCard card)
        , css
            [ position absolute
            , top (pct 50)
            , transforms
                [ translateX (rem (sin angle * 100))
                , translateY
                    (rem ((1.0 - cos angle) * 100))
                , rotate
                    (rad angle)
                ]
            , hover
                [ borderColor (rgb 0 255 0)
                ]
            ]
        ]
        [ viewCard card ]


viewHand : Array Card -> Html Msg
viewHand hand =
    div
        [ css
            [ justifyContent center
            , displayFlex
            , alignItems center
            , position relative
            ]
        ]
        (Array.toList (Array.indexedMap (viewHandCard (Array.length hand)) hand))


viewNoCard : Html Msg
viewNoCard =
    div
        [ css
            [ width (rem 10)
            , height (rem 15)
            , border (px 3)
            , borderStyle solid
            , borderColor (rgb 0 0 0)
            ]
        ]
        []


viewTabel : Array (Maybe Card) -> Html Msg
viewTabel table =
    div
        [ css
            [ justifyContent center
            , displayFlex
            , alignItems center
            , position relative
            ]
        ]
        (List.map
            (\x ->
                case x of
                    Nothing ->
                        viewNoCard

                    Just c ->
                        viewCard c
            )
            (Array.toList table)
        )


view : Model -> Unstyled.Html Msg
view model =
    toUnstyled (div [] [ viewTabel model.table, viewHand model.hand ])

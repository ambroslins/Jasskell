module Main exposing (..)

import Browser
import Css exposing (..)
import Html as Unstyled
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Suit
    = Bells
    | Hearts
    | Acorns
    | Leaves


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


type alias Card =
    { suit : Suit
    , rank : Rank
    }


showCard : Card -> String
showCard c =
    showSuit c.suit ++ " " ++ showRank c.rank


type alias Model =
    { hand : List Card
    , table : List (Maybe Card)
    }


init : Model
init =
    { hand =
        List.map (\r -> { suit = Bells, rank = r })
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
    , table = List.repeat 4 Nothing
    }


type Msg
    = RemoveCard Card


addCardToTable : Card -> List (Maybe Card) -> List (Maybe Card)
addCardToTable c cs =
    case cs of
        [] ->
            []

        Nothing :: xs ->
            Just c :: xs

        x :: xs ->
            x :: addCardToTable c xs


update : Msg -> Model -> Model
update msg model =
    case msg of
        RemoveCard c ->
            { model
                | hand = List.filter (\x -> x /= c) model.hand
                , table = addCardToTable c model.table
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


viewHand : List Card -> Html Msg
viewHand hand =
    div
        [ css
            [ justifyContent center
            , displayFlex
            , alignItems center
            , position relative
            ]
        ]
        (List.indexedMap (viewHandCard (List.length hand)) hand)


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


viewTabel : List (Maybe Card) -> Html Msg
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
            table
        )


view : Model -> Unstyled.Html Msg
view model =
    toUnstyled (div [] [ viewTabel model.table, viewHand model.hand ])

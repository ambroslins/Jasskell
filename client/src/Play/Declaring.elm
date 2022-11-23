module Play.Declaring exposing
    ( Model
    , Msg
    , State
    , decode
    , init
    , update
    , updateState
    , view
    )

import Hand exposing (Hand)
import Html exposing (Html, button, div, form, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, for, id, type_)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Seat exposing (Seat(..))
import User
import Variant exposing (Variant)
import Vector as Vector exposing (Vector)
import Vector.Index as Index exposing (Index(..))
import WebSocket



-- MODEL


type alias Model =
    { state : State
    , selectedVariant : Maybe Variant
    }


type alias State =
    { seats : Vector Seat
    , hand : Hand
    , eldest : Index
    , nominators : List Index
    }


init : State -> Model
init state =
    { state = state
    , selectedVariant = Nothing
    }



-- UPDATE


type Msg
    = SelectVariant Variant
    | DeclareVariant
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectVariant variant ->
            ( { model | selectedVariant = Just variant }, Cmd.none )

        DeclareVariant ->
            ( model
            , case model.selectedVariant of
                Just variant ->
                    WebSocket.send <|
                        Encode.object
                            [ ( "move"
                              , Encode.object
                                    [ ( "declare", Variant.encode variant ) ]
                              )
                            ]

                Nothing ->
                    Cmd.none
            )


updateState : State -> Model -> Model
updateState state model =
    { model | state = state }



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewSeat index attributes =
            let
                seat =
                    Vector.get index model.state.seats
            in
            div
                (attributes
                    ++ [ class "bg-gray-400 rounded-full border-4 text-4xl text-center"
                       , class "w-3/4 aspect-square"
                       ]
                )
                (case seat of
                    Empty ->
                        [ text (Index.toString index) ]

                    Taken user ->
                        [ text (User.name user) ]
                )
    in
    div []
        [ div
            [ class "grid grid-cols-3 grid-rows-3"
            , class "justify-center items-center justify-items-center"
            ]
            [ viewSeat Index1 [ class "row-span-3" ]
            , viewSeat Index2 []
            , viewSeat Index3 [ class "row-span-3" ]
            , if model.state.eldest == Index0 then
                viewVariantForm model

              else
                text "You are not eldest"
            , viewSeat Index0 []
            ]
        , Hand.view (always NoOp) model.state.hand
        ]


viewVariantForm : Model -> Html Msg
viewVariantForm model =
    let
        variantRadio variant =
            let
                identifier =
                    "declare-" ++ Variant.toString variant
            in
            li []
                [ input
                    [ type_ "radio"
                    , id identifier
                    , checked (model.selectedVariant == Just variant)
                    , onClick (SelectVariant variant)
                    , class "hidden peer"
                    ]
                    []
                , label
                    [ for identifier
                    , class "p-4 rounded-lg border-2 border-gray800 cursor-pointer peer-checked:border-green-500"
                    , class "inline-flex justify-between items-center w-full"
                    ]
                    [ div [ class "block text-xl w-full" ]
                        [ text (Variant.toString variant) ]
                    ]
                ]
    in
    form [ class "flex flex-col", onSubmit DeclareVariant ]
        [ ul [ class "grid grid-cols-4 gap-2" ]
            (List.map variantRadio Variant.all)
        , button [ class "text-2xl border-2 border-gray-800 shadow-md hover:bg-gray-400" ]
            [ text "Declare" ]
        ]



-- DECODER


decode : Decoder State
decode =
    Decode.succeed State
        |> Decode.required "seats" (Vector.decode Seat.decoder)
        |> Decode.required "hand" Hand.decode
        |> Decode.required "eldest" Index.decode
        |> Decode.required "nominators" (Decode.list Index.decode)

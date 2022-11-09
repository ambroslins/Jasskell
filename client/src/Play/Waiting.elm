module Play.Waiting exposing
    ( Model
    , Msg
    , State
    , decode
    , init
    , update
    , updateState
    , view
    )

import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (class, classList, disabled, for, id, placeholder, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Seat exposing (Seat(..))
import User
import Vector as Vector exposing (Vector)
import Vector.Index as Index exposing (Index(..))
import WebSocket



-- MODEL


type alias Model =
    { state : State
    }


type alias State =
    { seats : Vector Seat
    }


init : State -> Model
init state =
    { state = state
    }



-- UPDATE


type Msg
    = Start


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( model
            , WebSocket.send <|
                Encode.object
                    [ ( "start", Encode.null ) ]
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
    div
        [ class "grid grid-cols-3 grid-rows-3"
        , class "justify-center items-center justify-items-center"
        ]
        [ viewSeat Index1 [ class "row-span-3" ]
        , viewSeat Index2 []
        , viewSeat Index3 [ class "row-span-3" ]
        , button
            [ onClick Start
            , class "text-4xl p-4 m-4 rounded shadow hover:bg-green-300"
            ]
            [ text "Start Game" ]
        , viewSeat Index0 []
        ]



-- DECODER


decode : Decoder State
decode =
    Decode.succeed State
        |> Decode.required "seats" (Vector.decode Seat.decoder)

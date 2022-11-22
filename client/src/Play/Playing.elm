module Play.Playing exposing
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
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Seat exposing (Seat(..))
import User
import Vector as Vector exposing (Vector)
import Vector.Index as Index exposing (Index(..))



-- MODEL


type alias Model =
    { state : State
    }


type alias State =
    { seats : Vector Seat
    , hand : Hand
    , leader : Index
    }


init : State -> Model
init state =
    { state = state
    }



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


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
            , p []
                [ let
                    leader =
                        case Vector.get model.state.leader model.state.seats of
                            Seat.Empty ->
                                "empty"

                            Seat.Taken user ->
                                User.name user
                  in
                  text (leader ++ " is leader")
                ]
            , viewSeat Index0 []
            ]
        , Hand.view model.state.hand
        ]



-- DECODER


decode : Decoder State
decode =
    Decode.succeed State
        |> Decode.required "seats" (Vector.decode Seat.decoder)
        |> Decode.required "hand" Hand.decode
        |> Decode.required "leader" Index.decode

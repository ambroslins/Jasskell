module Play.Joining exposing
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
    , username : String
    , selected : Maybe Index
    }


type alias State =
    { seats : Vector Seat
    }


init : State -> Model
init state =
    { state = state
    , username = ""
    , selected = Nothing
    }



-- UPDATE


type Msg
    = ChangeUsername String
    | Select Index
    | TakeSeat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUsername username ->
            ( { model | username = username }, Cmd.none )

        Select index ->
            ( { model | selected = Just index }
            , Cmd.none
            )

        TakeSeat ->
            case model.selected of
                Nothing ->
                    ( model, Cmd.none )

                Just index ->
                    ( model
                    , WebSocket.send <|
                        Encode.object
                            [ ( "take-seat"
                              , Encode.object
                                    [ ( "username", Encode.string model.username )
                                    , ( "seat", Encode.int (Index.toInt index) )
                                    ]
                              )
                            ]
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

                isSelected =
                    Just index == model.selected
            in
            button
                (attributes
                    ++ [ onClick (Select index)
                       , disabled (seat /= Empty)
                       , class "bg-gray-400 rounded-full border-4 text-4xl text-center"
                       , class "w-3/4 aspect-square"
                       , classList [ ( "border-lime-400", isSelected ) ]
                       ]
                )
                (case seat of
                    Empty ->
                        [ text (Index.toString index) ]

                    Taken user ->
                        [ text (User.name user) ]
                )

        takeSeatForm =
            form
                [ onSubmit TakeSeat
                , class "shadow-md rounded p-8"
                ]
                [ div [ class "m-4" ]
                    [ label
                        [ for "username"
                        , class "block text-gray-700 font-bold mb-2 text-2xl"
                        ]
                        [ text "Username" ]
                    , input
                        [ type_ "text"
                        , id "username"
                        , placeholder "Username"
                        , onInput ChangeUsername
                        , class "shadow border rounded focus:shadow-outline p-2"
                        , class "text-2xl text-gray-700 "
                        ]
                        []
                    ]
                , div
                    [ class "flex flex-col items-center" ]
                    [ button
                        [ class "shadow border rounded text-xl text-center p-2"
                        , class "enabled:hover:bg-green-400"
                        , disabled <|
                            (model.selected == Nothing)
                                || String.isEmpty model.username
                        ]
                        [ text "Take Seat" ]
                    ]
                ]
    in
    div
        [ class "grid grid-cols-3 grid-rows-3"
        , class "justify-center items-center justify-items-center"
        ]
        [ viewSeat Index1 [ class "row-span-3" ]
        , viewSeat Index2 []
        , viewSeat Index3 [ class "row-span-3" ]
        , takeSeatForm
        , viewSeat Index0 []
        ]



-- DECODER


decode : Decoder State
decode =
    Decode.succeed State
        |> Decode.required "seats" (Vector.decode Seat.decoder)

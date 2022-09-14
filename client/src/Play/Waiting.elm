module Play.Waiting exposing (..)

import Circle
import Html exposing (Html, button, div, form, input, text)
import Html.Attributes exposing (class, classList, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Encode as Encode
import Seat exposing (Seat)
import Vector exposing (Vector)
import Vector.Index as Index exposing (Index(..))
import WebSocket



-- MODEL


type alias Model =
    { username : String
    , selectedSeat : Maybe Index
    }


init : Model
init =
    { username = ""
    , selectedSeat = Nothing
    }



-- UPDATE


type Msg
    = ChangeUsername String
    | SelectSeat Index
    | TakeSeat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUsername username ->
            ( { model | username = username }, Cmd.none )

        SelectSeat seat ->
            ( { model | selectedSeat = Just seat }, Cmd.none )

        TakeSeat ->
            case model.selectedSeat of
                Nothing ->
                    ( model, Cmd.none )

                Just seat ->
                    ( { model | selectedSeat = Nothing }
                    , WebSocket.send <|
                        Encode.object
                            [ ( "take-seat"
                              , Encode.object
                                    [ ( "username", Encode.string model.username )
                                    , ( "seat", Index.encode seat )
                                    ]
                              )
                            ]
                    )



-- VIEW


view : Vector Seat -> Model -> Html Msg
view seats model =
    let
        viewSeat i seat =
            let
                isSelected =
                    Just i == model.selectedSeat
            in
            { attributes = [ class "transition-transform duration-400" ]
            , key = String.fromInt <| Index.toInt i
            , child =
                button
                    [ onClick (SelectSeat i)
                    , class "bg-gray-400 rounded-full w-full h-full border-4 text-4xl text-center"
                    , classList [ ( "border-lime-400", isSelected ) ]
                    ]
                <|
                    case seat of
                        Seat.Empty ->
                            [ text <| String.fromInt <| Index.toInt i ]

                        Seat.Taken user ->
                            [ text user.name ]
            }
    in
    div [ class "justify-center items-center" ]
        [ div [ class "p-6" ]
            [ Circle.view
                { radius = 16.0
                , elementSize = { width = 8.0, height = 8.0 }
                , offset = Index.toInt <| Maybe.withDefault Index0 model.selectedSeat
                , center = text ""
                , elements =
                    Vector.toList <|
                        Vector.indexedMap
                            viewSeat
                            seats
                }
            ]
        , form
            [ onSubmit TakeSeat ]
            [ input
                [ type_ "text"
                , placeholder "username"
                , onInput ChangeUsername
                , class "text-xl p-2"
                ]
                []
            , button
                [ class "text-xl text-center p-2"
                , disabled <| model.selectedSeat == Nothing || String.isEmpty model.username
                ]
                [ text "Take Seat" ]
            ]
        ]

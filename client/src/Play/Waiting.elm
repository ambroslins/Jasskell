module Play.Waiting exposing (..)

import Circle
import Html exposing (Html, button, div, form, input, text)
import Html.Attributes exposing (class, classList, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import WebSocket



-- MODEL


type alias Model =
    { state : State
    , username : String
    , selected : Maybe Int
    }


type alias User =
    { name : String }


type alias State =
    { players : List (Maybe User)
    , status : Status
    }


type Status
    = Player
    | Guest


init : State -> Model
init state =
    { state = state
    , username = ""
    , selected = Nothing
    }



-- UPDATE


type Msg
    = ChangeUsername String
    | Select Int
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
                                    , ( "seat", Encode.int index )
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
        viewPlayer index player =
            let
                isSelected =
                    Just index == model.selected
            in
            { attributes = [ class "transition-transform duration-400" ]
            , key = String.fromInt index
            , child =
                button
                    [ onClick (Select index)
                    , disabled (player /= Nothing)
                    , class "bg-gray-400 rounded-full w-full h-full border-4 text-4xl text-center"
                    , classList [ ( "border-lime-400", isSelected ) ]
                    ]
                <|
                    case player of
                        Nothing ->
                            [ text <| String.fromInt index ]

                        Just user ->
                            [ text user.name ]
            }
    in
    div [ class "justify-center items-center" ]
        [ div [ class "p-6" ]
            [ Circle.view
                { radius = 16.0
                , elementSize = { width = 8.0, height = 8.0 }
                , center = text ""
                , elements =
                    List.indexedMap
                        viewPlayer
                        model.state.players
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
                , disabled <| model.selected == Nothing || String.isEmpty model.username
                ]
                [ text "Take Seat" ]
            ]
        ]



-- DECODER


decoder : Decoder State
decoder =
    let
        decodePhase =
            Decode.string
                |> Decode.andThen
                    (\phase ->
                        if phase == "waiting" then
                            Decode.succeed ()

                        else
                            Decode.fail "phase does not match waiting"
                    )

        decodeUsers =
            Decode.list
                (Decode.nullable
                    (Decode.succeed User
                        |> Decode.required "name" Decode.string
                    )
                )

        decodeStatus =
            Decode.string
                |> Decode.andThen
                    (\s ->
                        case s of
                            "player" ->
                                Decode.succeed Player

                            "guest" ->
                                Decode.succeed Guest

                            _ ->
                                Decode.fail ("Invalid status: " ++ s)
                    )
    in
    Decode.succeed (always State)
        |> Decode.required "phase" decodePhase
        |> Decode.required "users" decodeUsers
        |> Decode.required "status" decodeStatus

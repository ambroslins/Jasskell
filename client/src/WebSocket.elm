port module WebSocket exposing (Event(..), close, connect, receive, send)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required, resolve)
import Json.Encode as Encode


connect : String -> Cmd msg
connect address =
    sendSocket <|
        Encode.object
            [ ( "type", Encode.string "connect" )
            , ( "address", Encode.string address )
            ]


receive : (Result Decode.Error Event -> msg) -> Sub msg
receive f =
    receiveSocket (f << Decode.decodeValue decodeEvent)


send : Encode.Value -> Cmd msg
send data =
    sendSocket <|
        Encode.object
            [ ( "type", Encode.string "send" )
            , ( "data", data )
            ]


close : Cmd msg
close =
    sendSocket <|
        Encode.object [ ( "type", Encode.string "close" ) ]


type Event
    = Close
    | Error
    | Message Encode.Value
    | Open


decodeEvent : Decoder Event
decodeEvent =
    let
        event : String -> Maybe Encode.Value -> Decoder Event
        event t d =
            case t of
                "close" ->
                    Decode.succeed Close

                "error" ->
                    Decode.succeed Error

                "message" ->
                    case d of
                        Just v ->
                            Decode.succeed (Message v)

                        Nothing ->
                            Decode.fail "invalid data"

                "open" ->
                    Decode.succeed Open

                _ ->
                    Decode.fail ("unkown type: " ++ t)
    in
    Decode.succeed event
        |> required "type" Decode.string
        |> custom (Decode.maybe (Decode.field "data" Decode.value))
        |> resolve


port sendSocket : Encode.Value -> Cmd msg


port receiveSocket : (Encode.Value -> msg) -> Sub msg

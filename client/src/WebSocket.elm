port module WebSocket exposing
    ( Event(..)
    , close
    , open
    , send
    , subscribe
    )

import Json.Encode exposing (Value, null)


type Event
    = Open
    | Message String
    | Close
    | Error


port onOpen : (Value -> msg) -> Sub msg


port onMessage : (String -> msg) -> Sub msg


port onClose : (Value -> msg) -> Sub msg


port onError : (Value -> msg) -> Sub msg


subscribe : (Event -> msg) -> Sub msg
subscribe toMsg =
    Sub.map toMsg <|
        Sub.batch
            [ onOpen <| always Open
            , onMessage Message
            , onClose <| always Close
            , onError <| always Error
            ]


port send : Value -> Cmd msg


port open : String -> Cmd msg


port closePort : Value -> Cmd msg


close : Cmd msg
close =
    closePort null

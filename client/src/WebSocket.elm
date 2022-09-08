port module WebSocket exposing
    ( Event(..)
    , close
    , open
    , send
    , subscribe
    )


type Event
    = Open
    | Message String
    | Close
    | Error


port onOpen : msg -> Sub msg


port onMessage : (String -> msg) -> Sub msg


port onClose : msg -> Sub msg


port onError : msg -> Sub msg


subscribe : (Event -> msg) -> Sub msg
subscribe toMsg =
    Sub.map toMsg <|
        Sub.batch
            [ onOpen Open
            , onMessage Message
            , onClose Close
            , onError Error
            ]


port send : String -> Cmd msg


port open : String -> Cmd msg


port close : Cmd msg

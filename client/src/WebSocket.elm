port module WebSocket exposing (connect, receive, send)

import Json.Encode exposing (Value)


port connect : String -> Cmd msg


port receive : (Value -> msg) -> Sub msg


port send : Value -> Cmd msg

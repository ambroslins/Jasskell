module API exposing (createTable)

import Http
import TableID exposing (TableID)


createTable : (Result Http.Error TableID -> msg) -> Cmd msg
createTable toMsg =
    Http.post
        { url = "/api/table"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg TableID.decode
        }

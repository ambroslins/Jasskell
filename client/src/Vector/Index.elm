module Vector.Index exposing (Index(..), add, encode, fromInt, modulo, negate, sub, toInt)

import Json.Encode as Encode exposing (Value)


type Index
    = Index0
    | Index1
    | Index2
    | Index3


toInt : Index -> Int
toInt i =
    case i of
        Index0 ->
            0

        Index1 ->
            1

        Index2 ->
            2

        Index3 ->
            3


fromInt : Int -> Maybe Index
fromInt i =
    case i of
        0 ->
            Just Index0

        1 ->
            Just Index1

        2 ->
            Just Index2

        3 ->
            Just Index3

        _ ->
            Nothing


modulo : Int -> Index
modulo =
    Maybe.withDefault Index0 << fromInt << modBy 4


add : Index -> Index -> Index
add i j =
    modulo (toInt i + toInt j)


negate : Index -> Index
negate =
    modulo << Basics.negate << toInt


sub : Index -> Index -> Index
sub i j =
    modulo (toInt i - toInt j)


encode : Index -> Value
encode =
    Encode.int << toInt

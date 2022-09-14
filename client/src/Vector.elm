module Vector exposing
    ( Vector
    , decode
    , fromList
    , get
    , indexedMap
    , initialize
    , map
    , repeat
    , rotate
    , toList
    )

import Json.Decode as Decode exposing (Decoder)
import Vector.Index as Index exposing (Index(..))


type Vector a
    = Vector
        { i0 : a
        , i1 : a
        , i2 : a
        , i3 : a
        }


get : Index -> Vector a -> a
get i (Vector v) =
    case i of
        Index0 ->
            v.i0

        Index1 ->
            v.i1

        Index2 ->
            v.i2

        Index3 ->
            v.i3


fromList : List a -> Maybe (Vector a)
fromList xs =
    case xs of
        [ x0, x1, x2, x3 ] ->
            Just <| Vector { i0 = x0, i1 = x1, i2 = x2, i3 = x3 }

        _ ->
            Nothing


toList : Vector a -> List a
toList (Vector v) =
    [ v.i0, v.i1, v.i2, v.i3 ]


repeat : a -> Vector a
repeat x =
    Vector { i0 = x, i1 = x, i2 = x, i3 = x }


initialize : (Index -> a) -> Vector a
initialize f =
    Vector { i0 = f Index0, i1 = f Index1, i2 = f Index2, i3 = f Index3 }


rotate : Index -> Vector a -> Vector a
rotate n vec =
    initialize (\i -> get (Index.add i n) vec)


map : (a -> b) -> Vector a -> Vector b
map f =
    indexedMap (always f)


indexedMap : (Index -> a -> b) -> Vector a -> Vector b
indexedMap f (Vector v) =
    Vector
        { i0 = f Index0 v.i0
        , i1 = f Index1 v.i1
        , i2 = f Index2 v.i2
        , i3 = f Index3 v.i3
        }


decode : Decoder a -> Decoder (Vector a)
decode d =
    Decode.list d
        |> Decode.andThen
            (\xs ->
                case fromList xs of
                    Nothing ->
                        Decode.fail "Invalid list length"

                    Just v ->
                        Decode.succeed v
            )

module Card exposing (..)


type Suit
    = Bells
    | Hearts
    | Acorns
    | Leaves


showSuit : Suit -> String
showSuit s =
    case s of
        Bells ->
            "Bells"

        Hearts ->
            "Hearts"

        Acorns ->
            "Acorns"

        Leaves ->
            "Leaves"


type Rank
    = Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Under
    | Over
    | King
    | Ace


showRank : Rank -> String
showRank r =
    case r of
        Six ->
            "Six"

        Seven ->
            "Seven"

        Eight ->
            "Eight"

        Nine ->
            "Nine"

        Ten ->
            "Ten"

        Under ->
            "Under"

        Over ->
            "Over"

        King ->
            "King"

        Ace ->
            "Ace"


type alias Card =
    { suit : Suit
    , rank : Rank
    }


showCard : Card -> String
showCard c =
    showSuit c.suit ++ " " ++ showRank c.rank

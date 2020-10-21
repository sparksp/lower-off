module Problem exposing (Problem(..), random, string)

{-| Extra problems that might occur.
-}

import Random


type Problem
    = NoScrewgate
    | NoLanyard
    | NoQuickdraws
    | NoBelayerComms


random : Random.Generator Problem
random =
    Random.uniform
        NoScrewgate
        [ NoLanyard
        , NoQuickdraws
        , NoBelayerComms
        ]


string : Problem -> String
string problem =
    case problem of
        NoScrewgate ->
            "You have no spare screwgates on you."

        NoLanyard ->
            "You have forgotten your sling / lanyard."

        NoQuickdraws ->
            "You have run out of quickdraws."

        NoBelayerComms ->
            "You cannot see or hear your belayer."

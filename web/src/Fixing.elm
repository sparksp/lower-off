module Fixing exposing (Fixing(..), plural, singular)

{-| Fixing at the rock.
-}


type Fixing
    = Hanger -- Usually sharp edged
    | Bolt -- Eye Bolt or Eco Bolt, usually small
    | Staple -- Usually big


singular : Fixing -> String
singular fixing =
    case fixing of
        Hanger ->
            "hanger"

        Bolt ->
            "bolt"

        Staple ->
            "staple"


plural : Fixing -> String
plural fixing =
    case fixing of
        Hanger ->
            "hangers"

        Bolt ->
            "bolts"

        Staple ->
            "staples"

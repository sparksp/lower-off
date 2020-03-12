module Connector exposing (Connector(..), plural, singular)

{-| Where's the rope going?
-}


type Connector
    = NoConnector -- Naughty
    | SmallLink -- Like a chain link, won't fit a bight of rope
    | BigRing -- Easily fit a bight of rope through
    | Snapgate
    | Screwgate
    | RamsHorn


singular : Connector -> String
singular connector =
    case connector of
        NoConnector ->
            "no connector"

        SmallLink ->
            "a small chain link"

        BigRing ->
            "a large ring"

        Snapgate ->
            "a snapgate carabiner"

        Screwgate ->
            "a screwgate carabiner"

        RamsHorn ->
            "a rams horn"


plural : Connector -> String
plural connector =
    case connector of
        NoConnector ->
            "no connectors"

        SmallLink ->
            "small chain links"

        BigRing ->
            "big rings"

        Snapgate ->
            "snapgate carabiners"

        Screwgate ->
            "screwgate carabiners"

        RamsHorn ->
            "rams horns"

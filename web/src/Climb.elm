module Climb exposing (Climb(..), string)

{-| Style of the climb.
-}


type Climb
    = LeadAndClean
    | LeadAndSetup
    | Second -- Always assume clean
    | TopRope -- Always assume clean


string : Climb -> String
string climb =
    case climb of
        LeadAndSetup ->
            "You are leading this climb and others will be climbing it after you."

        LeadAndClean ->
            "You are leading this climb but no one is following you, please clean it when you've finished."

        Second ->
            "You are seconding this climb, please clean it when you've finished."

        TopRope ->
            "You are the last person to top-rope this climb, please clean it when you've finished."

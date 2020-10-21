module Climb exposing (Climb(..), string)

{-| Style of the climb.
-}


type Climb
    = LeadAndClean
    | LeadAndLead
    | LeadAndSecond
    | LeadAndTopRope
    | Second -- Always assume clean
    | TopRope -- Always assume clean


string : Climb -> String
string climb =
    case climb of
        LeadAndClean ->
            "You are the last person to lead this climb, please clean it when you've finished."

        LeadAndLead ->
            "You are leading this climb and others want to lead it after you."

        LeadAndSecond ->
            "You are leading this climb and others want to second it after you."

        LeadAndTopRope ->
            "You are leading this climb and others want to top-rope it after you."

        Second ->
            "You are seconding this climb, please clean it when you've finished."

        TopRope ->
            "You are the last person to top-rope this climb, please clean it when you've finished."

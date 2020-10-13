module Ui.Icons exposing
    ( Icon
    , next
    )

{-| Icons used in this project.
-}

import Html exposing (Html)
import Svg
import Svg.Attributes exposing (d, fill, viewBox)


{-| Icon
-}
type alias Icon msg =
    List (Svg.Attribute msg) -> Html msg


{-| Skip (fast-forward)
From [zondicons](https://www.zondicons.com/).
-}
next : Icon msg
next attributes =
    zondicon attributes
        "M1 5l9 5-9 5V5zm9 0l9 5-9 5V5z"



--- HELPERS


{-| For icons from [zondicons](https://www.zondicons.com/).
-}
zondicon : List (Svg.Attribute msg) -> String -> Html msg
zondicon attributes icon =
    Svg.svg (viewBox "0 0 20 20" :: fill "currentColor" :: attributes)
        [ Svg.path [ d icon ] [] ]

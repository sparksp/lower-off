module Ui.Icons exposing
    ( Icon
    , back
    , empty
    , next
    , refresh
    )

{-| Icons used in this project.
-}

import Html.Styled exposing (Html)
import Svg.Styled as Svg
import Svg.Styled.Attributes exposing (d, fill, viewBox)


{-| Icon
-}
type alias Icon msg =
    List (Svg.Attribute msg) -> Html msg


{-| Back (cheveron-left)

From [zondicons](https://www.zondicons.com/).

-}
back : List (Svg.Attribute msg) -> Html msg
back attributes =
    Svg.svg attributes
        [ Svg.path [ d "M7.05 9.293L6.343 10 12 15.657l1.414-1.414L9.172 10l4.242-4.243L12 4.343z" ] [] ]


{-| Empty icon showing nothing
-}
empty : Icon msg
empty attributes =
    Svg.svg (viewBox "0 0 20 20" :: attributes) []


{-| Skip (fast-forward)

From [zondicons](https://www.zondicons.com/).

-}
next : Icon msg
next attributes =
    zondicon attributes
        "M1 5l9 5-9 5V5zm9 0l9 5-9 5V5z"


{-| Refresh

From [zondicons](https://www.zpndicons.com/).

-}
refresh : Icon msg
refresh attributes =
    zondicon attributes
        "M10 3v2a5 5 0 0 0-3.54 8.54l-1.41 1.41A7 7 0 0 1 10 3zm4.95 2.05A7 7 0 0 1 10 17v-2a5 5 0 0 0 3.54-8.54l1.41-1.41zM10 20l-4-4 4-4v8zm0-12V0l4 4-4 4z"



--- HELPERS


{-| For icons from [zondicons](https://www.zondicons.com/).
-}
zondicon : List (Svg.Attribute msg) -> String -> Html msg
zondicon attributes icon =
    Svg.svg (viewBox "0 0 20 20" :: fill "currentColor" :: attributes)
        [ Svg.path [ d icon ] [] ]

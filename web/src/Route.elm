module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , toUrl
    )

import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser, int, oneOf, s)


type Route
    = Home
    | Gallery
    | GalleryItem Int
    | Scenario



-- HELPERS


href : Route -> Html.Attribute msg
href targetRoute =
    Attr.href (toUrl targetRoute)


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser


toUrl : Route -> String
toUrl page =
    case page of
        Home ->
            Url.Builder.absolute [] []

        Gallery ->
            Url.Builder.absolute [ "gallery", "1" ] []

        GalleryItem n ->
            Url.Builder.absolute [ "gallery", String.fromInt n ] []

        Scenario ->
            Url.Builder.absolute [ "scenario" ] []



-- INTERNAL


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Gallery (s "gallery")
        , Parser.map GalleryItem (s "gallery" </> int)
        , Parser.map Scenario (s "scenario")
        ]

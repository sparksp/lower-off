module Anchor exposing (Alt(..), Anchor(..), anchor, toHtml)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Keyed as Keyed
import Tailwind as TW
import Url exposing (Url)


type Alt
    = Alt String


type Anchor
    = Anchor
        { alt : Alt
        , src : Url
        }


anchor : Url -> Alt -> Anchor
anchor src alt =
    Anchor
        { alt = alt
        , src = src
        }


altString : Alt -> String
altString (Alt text) =
    text


toHtml : Anchor -> Html msg
toHtml (Anchor { alt, src }) =
    let
        src_ : String
        src_ =
            Url.toString src
    in
    Keyed.node
        "figure"
        [ Attr.class TW.wFull ]
        [ ( src_
          , Html.img
                [ Attr.class TW.wFull
                , Attr.src src_
                , Attr.alt (altString alt)
                ]
                []
          )
        ]

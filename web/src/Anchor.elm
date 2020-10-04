module Anchor exposing (Alt(..), Anchor(..), anchor, toHtml)

import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed
import Html.Tailwind as TW
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
    Html.Keyed.node
        "figure"
        [ TW.wFull ]
        [ ( src_
          , Html.img
                [ TW.wFull
                , Attr.src src_
                , Attr.alt (altString alt)
                ]
                []
          )
        ]

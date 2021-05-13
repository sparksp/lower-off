module Anchor exposing (Alt(..), Anchor(..), anchor, toHtml)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Keyed as Keyed
import Tailwind.Utilities as Tw
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
        [ Attr.css [ Tw.w_full ] ]
        [ ( src_
          , Html.img
                [ Attr.css [ Tw.w_full ]
                , Attr.src src_
                , Attr.alt (altString alt)
                ]
                []
          )
        ]

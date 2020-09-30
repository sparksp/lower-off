module Anchor exposing (Alt(..), Anchor(..), anchor, toElement)

import Element exposing (Element, fill, image, width)
import Element.Keyed exposing (el)
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


toElement : Anchor -> Element msg
toElement (Anchor { alt, src }) =
    let
        src_ : String
        src_ =
            Url.toString src
    in
    el [ width fill ]
        ( src_
        , image [ width fill ] { src = src_, description = altString alt }
        )

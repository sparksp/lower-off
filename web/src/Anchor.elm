module Anchor exposing (Alt(..), Anchor(..), Title(..), anchor, toElement)

import Element exposing (Element, fill, image, width)
import Element.Keyed exposing (el)
import Url exposing (Url)


type Title
    = Title String


type Alt
    = Alt String


type Anchor
    = Anchor
        { title : Title
        , alt : Alt
        , src : Url
        }


anchor : Url -> Title -> Alt -> Anchor
anchor src title alt =
    Anchor
        { title = title
        , alt = alt
        , src = src
        }


altString : Alt -> String
altString (Alt text) =
    text


toElement : Anchor -> Element msg
toElement (Anchor { alt, src }) =
    let
        src_ =
            Url.toString src
    in
    el [ width fill ]
        ( src_
        , image [ width fill ] { src = src_, description = altString alt }
        )

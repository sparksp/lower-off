module Anchor exposing (Alt(..), Anchor(..), Title(..), anchor, string, toElement)

import Element exposing (Element, fill, image, width)
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


string : Anchor -> String
string (Anchor { alt }) =
    altString alt


altString : Alt -> String
altString (Alt text) =
    text


toElement : Anchor -> Element msg
toElement (Anchor { alt, src }) =
    image [ width fill ] { src = Url.toString src, description = altString alt }

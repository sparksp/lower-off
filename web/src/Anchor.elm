module Anchor exposing (Alt(..), Anchor(..), anchor, string, toElement)

import Element exposing (Element, fill, image, width)
import Url exposing (Url)


type Alt
    = Alt String


type Anchor
    = Anchor
        { title : String
        , alt : Alt
        , src : Url
        }


anchor : Url -> Alt -> Anchor
anchor src alt =
    Anchor
        { title = "My Anchor"
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

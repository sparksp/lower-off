module Anchor.API exposing (fetch)

import Anchor
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as JDX
import Url exposing (Url)


fetch : String -> (Result Http.Error (List Anchor.Anchor) -> msg) -> Cmd msg
fetch baseUrl callback =
    Http.get
        { url = baseUrl ++ "/anchor/index.json"
        , expect = Http.expectJson callback feedDecoder
        }


feedDecoder : Decoder (List Anchor.Anchor)
feedDecoder =
    Decode.at [ "data", "items" ] anchorListDecoder


anchorListDecoder : Decoder (List Anchor.Anchor)
anchorListDecoder =
    Decode.list anchorDecoder


anchorDecoder : Decoder Anchor.Anchor
anchorDecoder =
    Decode.map2
        Anchor.anchor
        srcDecoder
        altDecoder


srcDecoder : Decoder Url
srcDecoder =
    Decode.at
        [ "image", "src" ]
        JDX.url


altDecoder : Decoder Anchor.Alt
altDecoder =
    Decode.map Anchor.Alt
        (Decode.oneOf
            [ Decode.at [ "image", "alt" ] Decode.string
            , Decode.at [ "image", "title" ] Decode.string
            ]
        )

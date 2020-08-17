module Anchor.API exposing (fetch)

import Anchor
import Http
import Json.Decode as JD exposing (Decoder)
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
    JD.at [ "data", "items" ] anchorListDecoder


anchorListDecoder : Decoder (List Anchor.Anchor)
anchorListDecoder =
    JD.list anchorDecoder


anchorDecoder : Decoder Anchor.Anchor
anchorDecoder =
    JD.map3
        Anchor.anchor
        srcDecoder
        titleDecoder
        altDecoder


srcDecoder : Decoder Url
srcDecoder =
    JD.at [ "image", "src" ] JDX.url


titleDecoder : Decoder Anchor.Title
titleDecoder =
    JD.map Anchor.Title (JD.field "title" JD.string)


altDecoder : Decoder Anchor.Alt
altDecoder =
    JD.map Anchor.Alt
        (JD.oneOf
            [ JD.at [ "image", "alt" ] JD.string
            , JD.at [ "image", "title" ] JD.string
            ]
        )

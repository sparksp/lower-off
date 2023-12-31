module Page.Gallery exposing (view)

import Action exposing (Action)
import Anchor exposing (Anchor)
import Browser.Styled exposing (Document)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import List.Extra
import Page.NotFound
import Route
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as TwTheme
import Tailwind.Utilities as Tw
import Ui.Icons


viewAnchor : Anchor -> Html msg
viewAnchor anchor =
    Html.div
        [ Attr.css
            [ Breakpoints.sm [ Tw.max_w_lg ]
            , Tw.w_full
            , Tw.bg_color TwTheme.white
            , Tw.shadow_md
            , Tw.mb_3
            ]
        ]
        [ Anchor.toHtml anchor
        ]


preloadNextAnchor : List Anchor -> Int -> Html msg
preloadNextAnchor anchors id =
    case List.Extra.getAt id anchors of
        Nothing ->
            Html.text ""

        Just anchor ->
            Html.div
                [ Attr.css
                    [ Tw.hidden
                    ]
                ]
                [ Anchor.toHtml anchor
                ]


viewNextButton : { id : Int, count : Int } -> Html msg
viewNextButton { id, count } =
    if id == count then
        Html.text ""

    else
        nextButton id


nextButton : Int -> Html msg
nextButton id =
    Html.div
        [ Attr.css
            [ Tw.pb_3
            , Tw.flex
            , Tw.flex_col
            , Tw.w_full
            , Breakpoints.sm [ Tw.max_w_lg ]
            ]
        ]
        [ Html.a
            [ Route.GalleryItem (id + 1) |> Route.href
            , Attr.css
                [ Tw.text_color TwTheme.black
                , Tw.flex
                , Tw.p_1
                , Tw.w_full
                ]
            ]
            [ Html.div
                [ Attr.css
                    [ Tw.flex_grow
                    , Tw.text_right
                    ]
                ]
                [ Html.text "Next Anchor"
                ]
            , Ui.Icons.next
                [ SvgAttr.css
                    [ Tw.w_6
                    , Tw.h_6
                    , Tw.ml_1
                    , Tw.flex_none
                    ]
                ]
            ]
        ]


nextAction : { id : Int, count : Int } -> Action msg
nextAction { id, count } =
    if count == id then
        Action.None

    else
        Action.Link Ui.Icons.next (Route.GalleryItem (id + 1))


view : List Anchor -> Int -> ( Document msg, Action msg )
view anchors id =
    case List.Extra.getAt (id - 1) anchors of
        Nothing ->
            Page.NotFound.view

        Just anchor ->
            ( { title = "Lower-off Gallery"
              , body =
                    [ Html.div
                        [ Attr.css
                            [ Tw.flex
                            , Tw.flex_col
                            , Tw.items_center
                            ]
                        ]
                        [ viewAnchor anchor
                        , viewNextButton
                            { id = id
                            , count = List.length anchors
                            }
                        ]
                    , preloadNextAnchor anchors id
                    ]
              }
            , nextAction
                { id = id
                , count = List.length anchors
                }
            )

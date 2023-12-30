module Page exposing (Page(..), view)

import Action exposing (Action)
import Browser.Styled exposing (Document)
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Route
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as TwTheme
import Tailwind.Utilities as Tw
import Ui.Icons


{-| Current active menu item
-}
type Page
    = Other
    | Menu
    | Gallery
    | Scenario


type Title
    = Title String


view : Page -> ( Document msg, Action msg ) -> Document msg
view page ( { title, body }, action ) =
    { title = title
    , body =
        [ Css.Global.global (Css.Global.body [ Tw.bg_color TwTheme.orange_100 ] :: Tw.globalStyles)
        , Html.div
            [ Attr.css
                [ Tw.min_h_screen
                , Tw.flex
                , Tw.flex_col
                ]
            ]
            (viewHeader (Title title) page action :: body)
        ]
    }


viewHeader : Title -> Page -> Action msg -> Html msg
viewHeader (Title title) page action =
    Html.h1
        [ Attr.css
            [ Tw.w_full
            , Tw.p_1
            , Tw.border_b
            , Tw.bg_color TwTheme.orange_600
            , Tw.text_color TwTheme.white
            , Breakpoints.sm [ Tw.mb_3 ]
            ]
        ]
        [ Html.div
            [ Attr.css
                [ Tw.w_full
                , Tw.text_xl
                , Tw.flex
                , Tw.flex_row
                , Tw.items_center
                , Tw.mx_auto
                ]
            ]
            [ viewMenuButton page
            , viewTitle title
            , viewAction action
            ]
        ]


viewMenuButton : Page -> Html msg
viewMenuButton page =
    case page of
        Menu ->
            Ui.Icons.empty
                [ SvgAttr.css
                    [ Tw.w_6
                    , Tw.h_6
                    , Tw.mx_1
                    , Tw.flex_none
                    ]
                ]

        _ ->
            Html.a
                [ Route.href Route.Home
                ]
                [ Ui.Icons.menu
                    [ SvgAttr.css
                        [ Tw.w_6
                        , Tw.h_6
                        , Tw.mx_1
                        , Tw.flex_none
                        ]
                    ]
                ]


viewTitle : String -> Html msg
viewTitle title =
    Html.div
        [ Attr.css
            [ Breakpoints.sm [ Tw.max_w_lg ]
            , Tw.w_full
            , Tw.flex
            , Tw.flex_row
            , Tw.items_center
            , Tw.mx_auto
            , Tw.text_center
            ]
        ]
        [ Html.span
            [ Attr.css [ Tw.flex_grow ]
            ]
            [ Html.text title
            ]
        ]


viewAction : Action msg -> Html msg
viewAction action =
    case action of
        Action.Event icon msg ->
            Html.button
                [ Events.onClick msg ]
                [ icon
                    [ SvgAttr.css
                        [ Tw.w_6
                        , Tw.h_6
                        , Tw.mx_1
                        , Tw.flex_none
                        ]
                    ]
                ]

        Action.Link icon route ->
            Html.a
                [ Route.href route ]
                [ icon
                    [ SvgAttr.css
                        [ Tw.w_6
                        , Tw.h_6
                        , Tw.mx_1
                        , Tw.flex_none
                        ]
                    ]
                ]

        Action.None ->
            Ui.Icons.empty
                [ SvgAttr.css
                    [ Tw.w_6
                    , Tw.h_6
                    , Tw.mx_1
                    , Tw.flex_none
                    ]
                ]

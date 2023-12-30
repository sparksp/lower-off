module Page.Gallery exposing (Model, init, toSession, view)

import Action exposing (Action)
import Anchor exposing (Anchor)
import Browser.Styled exposing (Document)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Route
import Session exposing (Session)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as TwTheme
import Tailwind.Utilities as Tw
import Ui.Icons


type Model
    = Model Session State


type State
    = NoAnchors
    | ShowAnchor (List Anchor) Anchor (List Anchor)


init : Session -> List Anchor -> Int -> Model
init session anchors id =
    case anchors of
        [] ->
            Model session NoAnchors

        firstAnchor :: remainingAnchors ->
            Model session (showId id (ShowAnchor [] firstAnchor remainingAnchors))


toSession : Model -> Session
toSession (Model session _) =
    session


showId : Int -> State -> State
showId id state =
    case state of
        NoAnchors ->
            NoAnchors

        ShowAnchor prev curr next ->
            let
                allAnchors : List Anchor
                allAnchors =
                    List.reverse (curr :: prev) ++ next
            in
            case List.take id allAnchors |> List.reverse of
                newCurrent :: newPrevious ->
                    ShowAnchor newPrevious newCurrent (List.drop id allAnchors)

                [] ->
                    NoAnchors


stateToId : State -> Int
stateToId state =
    case state of
        NoAnchors ->
            0

        ShowAnchor previous _ _ ->
            List.length previous + 1


viewTextLine : String -> Html msg
viewTextLine =
    Html.text >> List.singleton >> Html.p [ Attr.css [ Tw.my_2 ] ]


viewStatusMessage : String -> Html msg
viewStatusMessage message =
    Html.div
        [ Attr.css
            [ Tw.px_3
            , Tw.w_full
            ]
        ]
        [ viewTextLine message ]


viewAnchor : Anchor -> Html msg
viewAnchor anchor =
    Html.div []
        [ Anchor.toHtml anchor
        ]


viewCurrentAnchor : State -> Html msg
viewCurrentAnchor state =
    Html.div
        [ Attr.css
            [ Breakpoints.sm [ Tw.max_w_lg ]
            , Tw.w_full
            , Tw.bg_color TwTheme.white
            , Tw.shadow_md
            , Tw.mb_3
            ]
        ]
        (case state of
            NoAnchors ->
                [ viewStatusMessage "Oops! Something went wrong." ]

            ShowAnchor _ anchor _ ->
                [ viewAnchor anchor ]
        )


viewNextButton : State -> Html msg
viewNextButton state =
    case state of
        NoAnchors ->
            Html.text ""

        ShowAnchor _ _ [] ->
            Html.text ""

        ShowAnchor _ _ _ ->
            nextButton (stateToId state)


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


nextAction : State -> Action msg
nextAction state =
    case state of
        NoAnchors ->
            Action.None

        ShowAnchor _ _ [] ->
            Action.None

        ShowAnchor _ _ _ ->
            Action.Link Ui.Icons.next (Route.GalleryItem (stateToId state + 1))


view : Model -> ( Document msg, Action msg )
view (Model _ state) =
    ( { title = "Lower-off Gallery"
      , body =
            [ Html.div
                [ Attr.css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.items_center
                    ]
                ]
                [ viewCurrentAnchor state
                , viewNextButton state
                ]
            ]
      }
    , nextAction state
    )

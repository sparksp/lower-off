module Page.Gallery exposing (Model, Msg, init, toSession, update, view)

import Action exposing (Action)
import Anchor exposing (Anchor)
import Anchor.API
import Browser.Styled exposing (Document)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Http
import Session exposing (Session)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as TwTheme
import Tailwind.Utilities as Tw
import Ui.Icons


type Model
    = Model Session State


type State
    = Loading
    | Failure
    | ShowAnchor (List Anchor) Anchor (List Anchor)


type Msg
    = GotAnchors (Result Http.Error (List Anchor))
    | ShowNext


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session Loading
    , Anchor.API.fetch "./api" GotAnchors
    )


toSession : Model -> Session
toSession (Model session _) =
    session


showNext : State -> State
showNext state =
    case state of
        Loading ->
            Loading

        Failure ->
            Failure

        ShowAnchor prev curr (next :: rest) ->
            ShowAnchor (curr :: prev) next rest

        ShowAnchor _ _ [] ->
            state


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model session state) =
    case msg of
        GotAnchors (Ok (firstAnchor :: anchors)) ->
            ( Model session (ShowAnchor [] firstAnchor anchors)
            , Cmd.none
            )

        GotAnchors (Ok []) ->
            ( Model session Failure
            , Cmd.none
            )

        GotAnchors (Err _) ->
            ( Model session Failure
            , Cmd.none
            )

        ShowNext ->
            ( Model session (showNext state)
            , Cmd.none
            )


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


viewAnchor : Anchor -> Html Msg
viewAnchor anchor =
    Html.div []
        [ Anchor.toHtml anchor
        ]


viewCurrentAnchor : State -> Html Msg
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
            Loading ->
                [ viewStatusMessage "Please wait: racking up..." ]

            Failure ->
                [ viewStatusMessage "Oops! Something went wrong." ]

            ShowAnchor _ anchor _ ->
                [ viewAnchor anchor ]
        )


viewNextButton : State -> Html Msg
viewNextButton state =
    case state of
        Loading ->
            Html.div [] []

        Failure ->
            Html.div [] []

        ShowAnchor _ _ [] ->
            Html.div [] []

        ShowAnchor _ _ _ ->
            Html.div
                [ Attr.css
                    [ Tw.pb_3
                    , Tw.flex
                    , Tw.flex_col
                    , Tw.w_full
                    , Breakpoints.sm [ Tw.max_w_lg ]
                    ]
                ]
                [ Html.button
                    [ Events.onClick ShowNext
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


nextAction : State -> Action Msg
nextAction state =
    case state of
        Loading ->
            Action.None

        Failure ->
            Action.None

        ShowAnchor _ _ [] ->
            Action.None

        ShowAnchor _ _ _ ->
            Action.Icon Ui.Icons.next ShowNext


view : Model -> ( Document Msg, Action Msg )
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

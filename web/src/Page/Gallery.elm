module Page.Gallery exposing (Model, Msg, init, toSession, update, view)

import Action exposing (Action)
import Anchor exposing (Anchor)
import Anchor.API
import Browser.Styled exposing (Document)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Http
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
    = Loading Int
    | Failure
    | ShowAnchor (List Anchor) Anchor (List Anchor)


type Msg
    = GotAnchors (Result Http.Error (List Anchor))


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( Model session (Loading id)
    , Anchor.API.fetch "/api" GotAnchors
    )


toSession : Model -> Session
toSession (Model session _) =
    session


showId : Int -> State -> State
showId id state =
    case state of
        Loading _ ->
            Loading id

        Failure ->
            Failure

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
                    Failure


stateToId : State -> Int
stateToId state =
    case state of
        Loading id ->
            id

        Failure ->
            0

        ShowAnchor previous _ _ ->
            List.length previous + 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model session state) =
    case ( state, msg ) of
        ( Loading id, GotAnchors (Ok (firstAnchor :: anchors)) ) ->
            ( Model session (showId id (ShowAnchor [] firstAnchor anchors))
            , Cmd.none
            )

        ( _, GotAnchors (Ok (firstAnchor :: anchors)) ) ->
            ( Model session (ShowAnchor [] firstAnchor anchors)
            , Cmd.none
            )

        ( _, GotAnchors (Ok []) ) ->
            ( Model session Failure
            , Cmd.none
            )

        ( _, GotAnchors (Err _) ) ->
            ( Model session Failure
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
            Loading _ ->
                [ viewStatusMessage "Please wait: racking up..." ]

            Failure ->
                [ viewStatusMessage "Oops! Something went wrong." ]

            ShowAnchor _ anchor _ ->
                [ viewAnchor anchor ]
        )


viewNextButton : State -> Html Msg
viewNextButton state =
    case state of
        Loading _ ->
            Html.text ""

        Failure ->
            Html.text ""

        ShowAnchor _ _ [] ->
            Html.text ""

        ShowAnchor _ _ _ ->
            nextButton (stateToId state)


nextButton : Int -> Html Msg
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


nextAction : State -> Action Msg
nextAction state =
    case state of
        Loading _ ->
            Action.None

        Failure ->
            Action.None

        ShowAnchor _ _ [] ->
            Action.None

        ShowAnchor _ _ _ ->
            Action.Link Ui.Icons.next (Route.GalleryItem (stateToId state + 1))


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

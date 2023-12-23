module Main exposing (Model, Msg, State, main)

import Anchor exposing (Anchor)
import Anchor.API
import Browser
import Browser.Navigation as Nav
import Climb exposing (Climb)
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Http
import List.Extra
import Problem exposing (Problem)
import Random
import Scenario exposing (Scenario)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as TwTheme
import Tailwind.Utilities as Tw
import Ui.Icons
import Url



--- SCENARIOS


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , state : State
    }


type State
    = Loading
    | Failure
    | AnchorsReady (List Anchor)
    | ScenarioPick (List Anchor) Scenario


randomScenario : List Anchor -> Random.Generator Scenario
randomScenario anchors =
    Random.map3
        Scenario.new
        Climb.random
        (randomAnchor anchors)
        randomProblemList


randomProblemList : Random.Generator (List Problem)
randomProblemList =
    Random.weighted ( 100, 0 ) [ ( 10, 1 ) ]
        |> Random.andThen (\len -> Random.list len Problem.random)
        |> Random.map (List.Extra.uniqueBy Problem.string)


randomAnchor : List Anchor -> Random.Generator (Maybe Anchor)
randomAnchor anchors =
    randomListItem anchors


{-| Generate a random item from the given list. Returns `Nothing` if the list is empty.
-}
randomListItem : List a -> Random.Generator (Maybe a)
randomListItem list =
    Random.int 0 (List.length list - 1)
        |> Random.map (\n -> List.drop n list |> List.head)



--- PROGRAM


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key
      , url = url
      , state = Loading
      }
    , Anchor.API.fetch "./api" GotAnchors
    )


withAnchors : (List Anchor -> r) -> (() -> r) -> State -> r
withAnchors mapper default state =
    case state of
        Loading ->
            default ()

        Failure ->
            default ()

        AnchorsReady anchors ->
            mapper anchors

        ScenarioPick anchors _ ->
            mapper anchors


randomize : Model -> ( Model, Cmd Msg )
randomize model =
    model.state
        |> withAnchors
            (\anchors ->
                ( { model | state = AnchorsReady anchors }
                , Random.generate NewScenario (randomScenario anchors)
                )
            )
            (\() ->
                ( model, Cmd.none )
            )


setScenario : Scenario -> State -> State
setScenario scenario state =
    state
        |> withAnchors
            (\anchors ->
                ScenarioPick anchors scenario
            )
            (\() ->
                state
            )


type Msg
    = GotAnchors (Result Http.Error (List Anchor))
    | Randomize
    | NewScenario Scenario
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAnchors (Ok anchors) ->
            randomize { model | state = AnchorsReady anchors }

        GotAnchors (Err _) ->
            ( { model | state = Failure }
            , Cmd.none
            )

        Randomize ->
            randomize model

        NewScenario scenario ->
            ( { model | state = setScenario scenario model.state }
            , Cmd.none
            )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )


pageTitle : Html Msg
pageTitle =
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
        [ Html.button
            [ Attr.css
                [ Tw.w_full
                , Tw.text_xl
                ]
            , Events.onClick Randomize
            ]
            [ Html.div
                [ Attr.css
                    [ Tw.w_full
                    , Breakpoints.sm [ Tw.max_w_lg ]
                    , Tw.flex
                    , Tw.flex_row
                    , Tw.items_center
                    , Tw.mx_auto
                    ]
                ]
                [ Ui.Icons.empty
                    [ SvgAttr.css
                        [ Tw.w_6
                        , Tw.h_6
                        , Tw.mr_1
                        , Tw.flex_none
                        ]
                    ]
                , Html.span
                    [ Attr.css [ Tw.flex_grow ]
                    ]
                    [ Html.text "Lower-off Scenario"
                    ]
                , Ui.Icons.refresh
                    [ SvgAttr.css
                        [ Tw.w_6
                        , Tw.h_6
                        , Tw.ml_1
                        , Tw.flex_none
                        ]
                    ]
                ]
            ]
        ]


viewTextLine : String -> Html msg
viewTextLine =
    Html.text >> List.singleton >> Html.p [ Attr.css [ Tw.my_2 ] ]


viewClimb : Climb -> Html Msg
viewClimb =
    Climb.string >> viewTextLine


viewProblem : Problem -> Html Msg
viewProblem =
    Problem.string >> viewTextLine


noAnchor : Html Msg
noAnchor =
    viewTextLine "The anchor is missing."


viewScenario : Scenario -> List (Html Msg)
viewScenario scenario =
    [ Html.div
        [ Attr.css
            [ Tw.px_3
            , Tw.w_full
            ]
        ]
        (Scenario.mapClimb viewClimb scenario
            :: Scenario.mapProblems viewProblem scenario
            ++ [ viewTextLine "When get to the top of your climb you find..." ]
        )
    , Scenario.mapAnchor Anchor.toHtml scenario
        |> Maybe.withDefault noAnchor
    ]


viewRandomizeButton : State -> Html Msg
viewRandomizeButton state =
    case state of
        Loading ->
            Html.text ""

        Failure ->
            Html.text ""

        AnchorsReady _ ->
            randomizeButton

        ScenarioPick _ _ ->
            randomizeButton


randomizeButton : Html Msg
randomizeButton =
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
            [ Events.onClick Randomize
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
                [ Html.text "Next Scenario"
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


viewStatusMessage : String -> Html msg
viewStatusMessage message =
    Html.div
        [ Attr.css
            [ Tw.px_3
            , Tw.w_full
            ]
        ]
        [ viewTextLine message ]


viewRemoteScenario : State -> Html Msg
viewRemoteScenario state =
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

            AnchorsReady _ ->
                [ viewStatusMessage "Please wait: racking up..." ]

            ScenarioPick _ scenario ->
                viewScenario scenario
        )


view : Model -> Browser.Document Msg
view model =
    { title = "Lower-off Scenario"
    , body =
        [ Html.div
            [ Attr.css
                [ Tw.flex
                , Tw.flex_col
                , Tw.items_center
                ]
            ]
            [ Css.Global.global (Css.Global.body [ Tw.bg_color TwTheme.orange_100 ] :: Tw.globalStyles)
            , pageTitle
            , viewRemoteScenario model.state
            , viewRandomizeButton model.state
            ]
            |> Html.toUnstyled
        ]
    }

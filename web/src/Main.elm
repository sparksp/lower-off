module Main exposing (Model, Msg, main)

import Anchor exposing (Anchor)
import Anchor.API
import Browser
import Climb exposing (Climb)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import List.Extra
import Problem exposing (Problem)
import Random
import Svg.Attributes as SvgAttr
import Tailwind as TW
import Ui.Icons



--- SCENARIOS


type alias Model =
    State


type State
    = Loading
    | Failure
    | AnchorsReady (List Anchor)
    | ScenarioPick (List Anchor) Scenario


type Scenario
    = Scenario
        { climb : Climb
        , anchor : Maybe Anchor
        , problems : List Problem
        }


newScenario : Climb -> Maybe Anchor -> List Problem -> Scenario
newScenario climb anchor problems =
    Scenario { climb = climb, anchor = anchor, problems = problems }


randomScenario : List Anchor -> Random.Generator Scenario
randomScenario anchors =
    Random.map3
        newScenario
        Climb.random
        (randomAnchor anchors)
        randomProblemList


randomProblemList : Random.Generator (List Problem)
randomProblemList =
    Random.weighted ( 100, 0 ) [ ( 10, 1 ) ]
        |> Random.andThen (\len -> Random.list len Problem.random)
        |> Random.andThen (Random.constant << List.Extra.uniqueBy Problem.string)


randomAnchor : List Anchor -> Random.Generator (Maybe Anchor)
randomAnchor anchors =
    randomListItem anchors


{-| Generate a random item from the given list. Returns `Nothing` if the list is empty.
-}
randomListItem : List a -> Random.Generator (Maybe a)
randomListItem list =
    Random.int 0 (List.length list - 1)
        |> Random.andThen (Random.constant << (\n -> List.drop n list |> List.head))



--- PROGRAM


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Anchor.API.fetch "./api" GotAnchors
    )


withAnchors : (List Anchor -> r) -> (() -> r) -> Model -> r
withAnchors mapper default model =
    case model of
        AnchorsReady anchors ->
            mapper anchors

        ScenarioPick anchors _ ->
            mapper anchors

        _ ->
            default ()


randomize : Model -> ( Model, Cmd Msg )
randomize model =
    model
        |> withAnchors
            (\anchors ->
                ( AnchorsReady anchors, Random.generate NewScenario (randomScenario anchors) )
            )
            (\() ->
                ( model, Cmd.none )
            )


setScenario : Scenario -> Model -> Model
setScenario scenario model =
    model
        |> withAnchors
            (\anchors ->
                ScenarioPick anchors scenario
            )
            (\() ->
                model
            )


type Msg
    = GotAnchors (Result Http.Error (List Anchor))
    | Randomize
    | NewScenario Scenario


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAnchors (Ok anchors) ->
            randomize (AnchorsReady anchors)

        GotAnchors (Err _) ->
            ( Failure, Cmd.none )

        Randomize ->
            randomize model

        NewScenario scenario ->
            ( setScenario scenario model, Cmd.none )


pageTitle : Html Msg
pageTitle =
    Html.h1
        [ Attr.class TW.wFull
        , Attr.class TW.p1
        , Attr.class TW.borderB
        , Attr.class TW.bgOrange600
        , Attr.class TW.textWhite
        , Attr.class TW.smMb3
        ]
        [ Html.button
            [ Attr.class TW.wFull
            , Attr.class TW.textXl
            , Events.onClick Randomize
            ]
            [ Html.div
                [ Attr.class TW.wFull
                , Attr.class TW.smMaxWLg
                , Attr.class TW.flex
                , Attr.class TW.flexRow
                , Attr.class TW.itemsCenter
                , Attr.class TW.mxAuto
                ]
                [ Ui.Icons.empty
                    [ SvgAttr.class TW.w6
                    , SvgAttr.class TW.h6
                    , SvgAttr.class TW.mr1
                    , SvgAttr.class TW.flexNone
                    ]
                , Html.span
                    [ Attr.class TW.flexGrow
                    ]
                    [ Html.text "Lower-off Scenario"
                    ]
                , Ui.Icons.refresh
                    [ SvgAttr.class TW.w6
                    , SvgAttr.class TW.h6
                    , SvgAttr.class TW.ml1
                    , SvgAttr.class TW.flexNone
                    ]
                ]
            ]
        ]


viewTextLine : String -> Html msg
viewTextLine =
    Html.text >> List.singleton >> Html.p [ Attr.class TW.my2 ]


viewClimb : Climb -> Html Msg
viewClimb =
    Climb.string >> viewTextLine


viewProblem : Problem -> Html Msg
viewProblem =
    Problem.string >> viewTextLine


viewAnchor : Maybe Anchor -> Html Msg
viewAnchor maybeAnchor =
    case maybeAnchor of
        Just anchor ->
            Anchor.toHtml anchor

        Nothing ->
            viewTextLine "The anchor is missing."


viewScenario : Scenario -> List (Html Msg)
viewScenario (Scenario s) =
    [ Html.div
        [ Attr.class TW.px3
        , Attr.class TW.wFull
        ]
        (viewClimb s.climb
            :: List.map viewProblem s.problems
            ++ [ viewTextLine "When get to the top of your climb you find..." ]
        )
    , viewAnchor s.anchor
    ]


viewRandomizeButton : Model -> Html Msg
viewRandomizeButton model =
    case model of
        Failure ->
            Html.text ""

        Loading ->
            Html.text ""

        _ ->
            Html.div
                [ Attr.class TW.pb3
                , Attr.class TW.flex
                , Attr.class TW.flexCol
                , Attr.class TW.wFull
                , Attr.class TW.smMaxWLg
                ]
                [ Html.button
                    [ Events.onClick Randomize
                    , Attr.class TW.textBlack
                    , Attr.class TW.flex
                    , Attr.class TW.p1
                    , Attr.class TW.wFull
                    ]
                    [ Html.div
                        [ Attr.class TW.flexGrow
                        , Attr.class TW.textRight
                        ]
                        [ Html.text "Next Scenario"
                        ]
                    , Ui.Icons.next
                        [ SvgAttr.class TW.w6
                        , SvgAttr.class TW.h6
                        , SvgAttr.class TW.ml1
                        , SvgAttr.class TW.flexNone
                        ]
                    ]
                ]


viewStatusMessage : String -> Html msg
viewStatusMessage message =
    Html.div
        [ Attr.class TW.px3
        , Attr.class TW.wFull
        ]
        [ viewTextLine message ]


viewRemoteScenario : Model -> Html Msg
viewRemoteScenario model =
    Html.div
        [ Attr.class TW.smMaxWLg
        , Attr.class TW.wFull
        , Attr.class TW.bgWhite
        , Attr.class TW.shadowMd
        , Attr.class TW.mb3
        ]
        (case model of
            Loading ->
                [ viewStatusMessage "Please wait: racking up..." ]

            AnchorsReady _ ->
                [ viewStatusMessage "Please wait: racking up..." ]

            Failure ->
                [ viewStatusMessage "Oops! Something went wrong." ]

            ScenarioPick _ scenario ->
                viewScenario scenario
        )


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.class TW.flex
        , Attr.class TW.flexCol
        , Attr.class TW.itemsCenter
        ]
        [ pageTitle
        , viewRemoteScenario model
        , viewRandomizeButton model
        ]

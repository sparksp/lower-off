module Main exposing (Model, Msg, main)

import Anchor exposing (Anchor)
import Anchor.API
import Browser
import Climb exposing (Climb)
import Html exposing (Html)
import Html.Events as Events
import Html.Tailwind as TW
import Http
import List.Extra
import Problem exposing (Problem)
import Random
import Svg.Tailwind as SvgTW
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
        randomClimb
        (randomAnchor anchors)
        randomProblemList


randomProblemList : Random.Generator (List Problem)
randomProblemList =
    Random.weighted ( 100, 0 ) [ ( 10, 1 ) ]
        |> Random.andThen (\len -> Random.list len randomProblem)
        |> Random.andThen (Random.constant << List.Extra.uniqueBy Problem.string)


randomProblem : Random.Generator Problem
randomProblem =
    Random.uniform
        Problem.NoScrewgate
        [ Problem.NoLanyard
        , Problem.NoQuickdraws
        , Problem.NoBelayerComms
        ]


randomClimb : Random.Generator Climb
randomClimb =
    Random.uniform
        Climb.LeadAndClean
        [ Climb.LeadAndLead
        , Climb.LeadAndSecond
        , Climb.LeadAndTopRope
        , Climb.Second
        , Climb.TopRope
        ]


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
        [ TW.wFull
        , TW.p1
        , TW.borderB
        , TW.bgOrange600
        , TW.textWhite
        , TW.smMb3
        ]
        [ Html.button
            [ TW.wFull
            , TW.textXl
            , Events.onClick Randomize
            ]
            [ Html.div
                [ TW.wFull
                , TW.smMaxWLg
                , TW.flex
                , TW.flexRow
                , TW.itemsCenter
                , TW.mxAuto
                ]
                [ Ui.Icons.empty
                    [ SvgTW.w6
                    , SvgTW.h6
                    , SvgTW.mr1
                    , SvgTW.flexNone
                    ]
                , Html.span
                    [ TW.flexGrow
                    ]
                    [ Html.text "Lower-off Scenario"
                    ]
                , Ui.Icons.refresh
                    [ SvgTW.w6
                    , SvgTW.h6
                    , SvgTW.ml1
                    , SvgTW.flexNone
                    ]
                ]
            ]
        ]


viewTextLine : String -> Html msg
viewTextLine =
    Html.text >> List.singleton >> Html.p [ TW.my2 ]


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
        [ TW.px3
        , TW.wFull
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
                [ TW.pb3
                , TW.flex
                , TW.flexCol
                , TW.wFull
                , TW.smMaxWLg
                ]
                [ Html.button
                    [ Events.onClick Randomize
                    , TW.textBlack
                    , TW.flex
                    , TW.p1
                    , TW.wFull
                    ]
                    [ Html.div
                        [ TW.flexGrow
                        , TW.textRight
                        ]
                        [ Html.text "Next Scenario"
                        ]
                    , Ui.Icons.next
                        [ SvgTW.w6
                        , SvgTW.h6
                        , SvgTW.ml1
                        , SvgTW.flexNone
                        ]
                    ]
                ]


viewStatusMessage : String -> Html msg
viewStatusMessage message =
    Html.div
        [ TW.px3
        , TW.wFull
        ]
        [ viewTextLine message ]


viewRemoteScenario : Model -> Html Msg
viewRemoteScenario model =
    Html.div
        [ TW.smMaxWLg
        , TW.wFull
        , TW.bgWhite
        , TW.shadowMd
        , TW.mb3
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
        [ TW.flex
        , TW.flexCol
        , TW.itemsCenter
        ]
        [ pageTitle
        , viewRemoteScenario model
        , viewRandomizeButton model
        ]

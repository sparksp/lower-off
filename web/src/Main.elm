module Main exposing (main)

import Anchor exposing (Anchor)
import Anchor.API
import Browser
import Climb exposing (Climb)
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Http
import List.Extra
import Problem exposing (Problem)
import Random



--- SCENARIOS


type alias Model =
    State


type State
    = Loading
    | Failure Http.Error
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
    Random.uniform Problem.NoScrewgate [ Problem.NoLanyard, Problem.NoQuickdraws, Problem.NoBelayerComms ]


randomClimb : Random.Generator Climb
randomClimb =
    Random.uniform Climb.LeadAndClean [ Climb.LeadAndSetup, Climb.Second, Climb.TopRope ]


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
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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

        GotAnchors (Err error) ->
            ( Failure error, Cmd.none )

        Randomize ->
            randomize model

        NewScenario scenario ->
            ( setScenario scenario model, Cmd.none )


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top = 0, right = 0, bottom = 0, left = 0 }


pageTitle : Element Msg
pageTitle =
    El.row
        [ El.width El.fill
        , El.padding 5
        , Border.widthEach { edges | bottom = 1 }
        , Background.color <| El.rgb255 222 100 26
        , Font.color <| El.rgb255 255 255 255
        ]
        [ Input.button [ El.width El.fill, Font.size 32, Font.center, Region.heading 1 ]
            { onPress = Just Randomize
            , label = El.text "Lower-off Scenario"
            }
        ]


viewClimb : Climb -> Element Msg
viewClimb climb =
    El.paragraph [] [ El.text (Climb.string climb) ]


viewAnchor : Maybe Anchor -> Element Msg
viewAnchor maybeAnchor =
    case maybeAnchor of
        Just anchor ->
            Anchor.toElement anchor

        Nothing ->
            El.paragraph [] [ El.text "The anchor is missing." ]


listProblems : List Problem -> Element Msg
listProblems problems =
    El.column [ El.spacing 5 ] (List.map viewProblem problems)


viewProblem : Problem -> Element Msg
viewProblem problem =
    El.paragraph [] [ El.text (Problem.string problem) ]


viewScenario : Scenario -> Element Msg
viewScenario (Scenario s) =
    El.textColumn [ El.width El.fill, El.spacing 5 ]
        [ viewClimb s.climb
        , El.paragraph [] [ El.text "When get to the top of your climb you find..." ]
        , listProblems s.problems
        , viewAnchor s.anchor
        ]


viewRandomizeButton : Model -> Element Msg
viewRandomizeButton model =
    case model of
        Failure _ ->
            El.none

        Loading ->
            El.none

        _ ->
            El.row [ El.width El.fill, El.alignBottom, El.padding 5 ]
                [ Input.button
                    [ Background.color <| El.rgb255 36 160 237
                    , Border.color <| El.rgb255 200 200 200
                    , Border.rounded 3
                    , Border.width 1
                    , El.centerX
                    , El.padding 5
                    , El.width (El.maximum 800 El.fill)
                    , Font.center
                    , Font.color <| El.rgb255 255 255 255
                    ]
                    { onPress = Just Randomize, label = El.text "New Scenario" }
                ]


viewRemoteScenario : Model -> Element Msg
viewRemoteScenario model =
    El.row [ El.width (El.maximum 800 El.fill), El.centerX ]
        [ El.column [ El.width El.fill, El.padding 5, El.spacing 5 ]
            [ case model of
                Loading ->
                    El.paragraph [ Font.center ] [ El.text "Please wait: racking up..." ]

                Failure error ->
                    El.paragraph [] [ El.text "Oops! Something went wrong: ", El.text (Debug.toString error) ]

                AnchorsReady _ ->
                    El.none

                ScenarioPick _ scenario ->
                    viewScenario scenario
            ]
        ]


view : Model -> Html Msg
view model =
    El.layout []
        (El.column [ El.width El.fill, El.height El.fill ]
            [ pageTitle
            , viewRemoteScenario model
            , viewRandomizeButton model
            ]
        )

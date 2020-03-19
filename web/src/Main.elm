module Main exposing (main)

import Anchor exposing (Anchor)
import Anchor.API
import Browser
import Climb exposing (Climb)
import Element exposing (Element, column, fill, maximum, padding, paragraph, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Http
import List.Extra
import Problem exposing (Problem)
import Random
import RemoteData exposing (RemoteData)



--- SCENARIOS


type alias Model =
    { anchors : RemoteData Http.Error (List Anchor)
    , scenario : RemoteData String Scenario
    }


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


loadAnchors : Model -> ( Model, Cmd Msg )
loadAnchors model =
    ( { model
        | anchors = RemoteData.Loading
        , scenario = RemoteData.NotAsked
      }
    , Anchor.API.fetch "./api" GotAnchors
    )


init : () -> ( Model, Cmd Msg )
init _ =
    loadAnchors
        { anchors = RemoteData.NotAsked
        , scenario = RemoteData.NotAsked
        }


randomize : Model -> ( Model, Cmd Msg )
randomize model =
    case model.anchors of
        RemoteData.Success anchors ->
            ( { model | scenario = RemoteData.Loading }
            , Random.generate NewScenario (randomScenario anchors)
            )

        _ ->
            ( model, Cmd.none )


type Msg
    = GotAnchors (Result Http.Error (List Anchor))
    | Randomize
    | NewScenario Scenario


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAnchors (Ok anchors) ->
            randomize { model | anchors = RemoteData.Success anchors }

        GotAnchors (Err error) ->
            ( { model | anchors = RemoteData.Failure error }, Cmd.none )

        Randomize ->
            randomize model

        NewScenario scenario ->
            ( { model | scenario = RemoteData.Success scenario }, Cmd.none )


pageTitle : Element Msg
pageTitle =
    paragraph [ width fill, Font.size 32, Region.heading 1 ]
        [ text "Lower-off Scenario" ]


viewClimb : Climb -> Element Msg
viewClimb climb =
    paragraph [] [ text (Climb.string climb) ]


viewAnchor : Maybe Anchor -> Element Msg
viewAnchor maybeAnchor =
    case maybeAnchor of
        Just anchor ->
            Anchor.toElement anchor

        Nothing ->
            paragraph [] [ text "The anchor is missing." ]


listProblems : List Problem -> Element Msg
listProblems problems =
    Element.textColumn [ spacing 10 ] (List.map viewProblem problems)


viewProblem : Problem -> Element Msg
viewProblem problem =
    paragraph [] [ text (Problem.string problem) ]


viewScenario : Scenario -> Element Msg
viewScenario (Scenario s) =
    Element.textColumn [ width fill, spacing 10 ]
        [ viewClimb s.climb
        , paragraph [] [ text "You get to the top of your climb and find..." ]
        , listProblems s.problems
        , viewAnchor s.anchor
        ]


viewRandomizeButton : RemoteData e a -> Element Msg
viewRandomizeButton remote =
    let
        action =
            case remote of
                RemoteData.Success _ ->
                    Just Randomize

                _ ->
                    Nothing
    in
    Input.button
        [ padding 5
        , Border.width 1
        , Border.rounded 3
        , Border.color <| Element.rgb255 200 200 200
        ]
        { onPress = action, label = text "Randomize" }


viewRemoteScenario : RemoteData String Scenario -> Element Msg
viewRemoteScenario remoteModel =
    case remoteModel of
        RemoteData.NotAsked ->
            paragraph [] [ text "" ]

        RemoteData.Loading ->
            paragraph [] [ text "Shuffling..." ]

        RemoteData.Failure error ->
            paragraph [] [ text "Oops! Something went wrong: ", text error ]

        RemoteData.Success scenario ->
            viewScenario scenario


view : Model -> Html Msg
view model =
    Element.layout []
        (column [ width (maximum 800 fill), padding 10, spacing 10 ]
            [ pageTitle
            , viewRemoteScenario model.scenario
            , viewRandomizeButton model.scenario
            ]
        )

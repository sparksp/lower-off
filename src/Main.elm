module Main exposing (main)

import Anchor exposing (Anchor)
import Browser
import ChainConnector exposing (ChainConnector)
import Connector exposing (Connector)
import Element exposing (Element, column, fill, padding, paragraph, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Fixing exposing (Fixing)
import Html exposing (Html)
import List.Extra
import Problem exposing (Problem)
import Random



--- SCENARIOS


type Scenario
    = Scenario
        { climb : Climb
        , anchor : Anchor
        , problems : List Problem
        }


{-| Style of the climb.
-}
type Climb
    = LeadAndClean
    | LeadAndSetup
    | Second -- Always assume clean
    | TopRope -- Always assume clean


{-| What condition is the equipment in?
-}
type Condition e
    = Good e
    | Broken e
    | Rusty e
    | Worn e


randomProblemList : Random.Generator (List Problem)
randomProblemList =
    Random.int 0 4
        |> Random.andThen (\len -> Random.list len randomProblem)
        |> Random.andThen (Random.constant << List.Extra.uniqueBy Problem.string)


randomProblem : Random.Generator Problem
randomProblem =
    Random.uniform Problem.NoScrewgate [ Problem.NoLanyard, Problem.NoQuickdraws, Problem.NoBelayerComms ]


randomClimb : Random.Generator Climb
randomClimb =
    Random.uniform LeadAndClean [ LeadAndSetup, Second, TopRope ]


randomAnchor : Random.Generator Anchor
randomAnchor =
    Random.uniform
        (Random.constant Anchor.None)
        [ Random.map2 Anchor.Single randomFixing randomChainConnector
        , Random.map2 Anchor.Twin randomFixing randomChainConnector
        , Random.map2 Anchor.Joined randomFixing randomConnector
        ]
        |> Random.andThen identity


randomFixing : Random.Generator Fixing
randomFixing =
    Random.uniform Fixing.Hanger [ Fixing.Bolt, Fixing.Staple ]


randomChainConnector : Random.Generator ChainConnector
randomChainConnector =
    Random.uniform ChainConnector.NoChain [ ChainConnector.Chain ]
        |> Random.andThen
            (\chain -> Random.map chain randomConnector)


randomConnector : Random.Generator Connector
randomConnector =
    Random.uniform
        Connector.NoConnector
        [ Connector.BigRing
        , Connector.RamsHorn
        , Connector.Screwgate
        , Connector.SmallLink
        , Connector.Snapgate
        ]



--- PROGRAM


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


type alias Model =
    Scenario


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initScenario, Cmd.none )


initScenario : Scenario
initScenario =
    Scenario
        { climb = LeadAndClean
        , anchor = Anchor.Joined Fixing.Bolt Connector.BigRing
        , problems = []
        }


randomize : Cmd Msg
randomize =
    Cmd.batch
        [ Random.generate NewProblems randomProblemList
        , Random.generate NewClimb randomClimb
        , Random.generate NewAnchor randomAnchor
        ]


type Msg
    = Randomize
    | NewAnchor Anchor
    | NewClimb Climb
    | NewProblems (List Problem)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Scenario scenario) =
    case msg of
        Randomize ->
            ( Scenario scenario, randomize )

        NewAnchor anchor ->
            ( Scenario { scenario | anchor = anchor }, Cmd.none )

        NewClimb climb ->
            ( Scenario { scenario | climb = climb }, Cmd.none )

        NewProblems problems ->
            ( Scenario { scenario | problems = problems }, Cmd.none )


pageTitle : Element Msg
pageTitle =
    paragraph [ width fill, Font.size 32, Region.heading 1 ]
        [ text "Lower-off Scenario" ]


viewClimb : Climb -> Element Msg
viewClimb climb =
    paragraph []
        [ case climb of
            LeadAndSetup ->
                text "You are leading this climb and others will be climbing it after you."

            LeadAndClean ->
                text "You are leading this climb but no one is following you, please clean it when you've finished."

            Second ->
                text "You are seconding this climb, please clean it when you've finished."

            TopRope ->
                text "You are the last person to top-rope this climb, please clean it when you've finished."
        ]


viewAnchor : Anchor -> Element Msg
viewAnchor anchor =
    paragraph [] [ text ("You get to the top of your climb and find " ++ Anchor.string anchor) ]


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
        , viewAnchor s.anchor
        , listProblems s.problems
        ]


viewRandomizeButton : Element Msg
viewRandomizeButton =
    Input.button
        [ padding 5
        , Border.width 1
        , Border.rounded 3
        , Border.color <| Element.rgb255 200 200 200
        ]
        { onPress = Just Randomize, label = text "Randomize" }


view : Model -> Html Msg
view model =
    Element.layout []
        (column [ width fill, padding 10, spacing 10 ]
            [ pageTitle
            , viewRandomizeButton
            , viewScenario model
            ]
        )

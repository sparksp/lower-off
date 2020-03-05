module Main exposing (main)

import Anchor exposing (Anchor)
import Browser
import Connector
import Element exposing (Element, column, fill, padding, paddingXY, paragraph, spacing, text, width)
import Element.Font as Font
import Element.Region as Region
import Fixing
import Html exposing (Html)



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


{-| Extra problems that might occur.
-}
type Problem
    = NoScrewgate
    | NoLanyard
    | NoQuickdraws
    | NoBelayerComms



--- PROGRAM


main : Program () Model Msg
main =
    Browser.sandbox { init = initScenario, update = update, view = view }


type alias Model =
    Scenario


initScenario : Scenario
initScenario =
    Scenario
        { climb = LeadAndClean
        , anchor = Anchor.Joined Fixing.Bolt Connector.BigRing
        , problems = [ NoScrewgate, NoLanyard, NoQuickdraws, NoBelayerComms ]
        }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


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
    paragraph [] [ text ("You get to the top of your climb and find " ++ Anchor.singular anchor) ]


listProblems : List Problem -> Element Msg
listProblems problems =
    Element.textColumn [ spacing 10 ] (List.map viewProblem problems)


viewProblem : Problem -> Element Msg
viewProblem problem =
    case problem of
        NoScrewgate ->
            paragraph [] [ text "You have no spare screwgates on you." ]

        NoLanyard ->
            paragraph [] [ text "You've forgotten your sling / lanyard." ]

        NoQuickdraws ->
            paragraph [] [ text "You have run out of quickdraws." ]

        NoBelayerComms ->
            paragraph [] [ text "You cannot see or hear your belayer." ]


viewScenario : Scenario -> Element Msg
viewScenario (Scenario s) =
    Element.textColumn [ width fill, spacing 10, paddingXY 0 10 ]
        [ viewClimb s.climb
        , viewAnchor s.anchor
        , listProblems s.problems
        ]


view : Model -> Html Msg
view model =
    Element.layout [ padding 10 ]
        (column [ width fill ]
            [ pageTitle
            , viewScenario model
            ]
        )

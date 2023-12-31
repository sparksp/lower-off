module Scenario exposing (Scenario, mapAnchor, mapClimb, mapProblems, new)

import Anchor exposing (Anchor)
import Climb exposing (Climb)
import Problem exposing (Problem)


type Scenario
    = Scenario
        { climb : Climb
        , anchor : Maybe Anchor
        , problems : List Problem
        }


new : Climb -> Maybe Anchor -> List Problem -> Scenario
new newClimb newAnchor problemList =
    Scenario
        { climb = newClimb
        , anchor = newAnchor
        , problems = problemList
        }


mapAnchor : (Anchor -> x) -> Scenario -> Maybe x
mapAnchor fn (Scenario s) =
    Maybe.map fn s.anchor


mapClimb : (Climb -> x) -> Scenario -> x
mapClimb fn (Scenario s) =
    fn s.climb


mapProblems : (Problem -> x) -> Scenario -> List x
mapProblems fn (Scenario s) =
    List.map fn s.problems

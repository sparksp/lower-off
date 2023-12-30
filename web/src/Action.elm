module Action exposing (Action(..))

import Route
import Ui.Icons


type Action msg
    = Event (Ui.Icons.Icon msg) msg
    | Link (Ui.Icons.Icon msg) Route.Route
    | None

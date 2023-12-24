module Action exposing (Action(..))

import Ui.Icons


type Action msg
    = Icon (Ui.Icons.Icon msg) msg
    | None

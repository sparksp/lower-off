module Page.Blank exposing (view)

import Action exposing (Action)
import Browser.Styled exposing (Document)


view : ( Document msg, Action msg )
view =
    ( { title = ""
      , body = []
      }
    , Action.None
    )

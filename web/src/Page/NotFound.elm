module Page.NotFound exposing (view)

import Action exposing (Action)
import Browser.Styled exposing (Document)
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Tailwind.Theme as TwTheme
import Tailwind.Utilities as Tw


view : ( Document msg, Action msg )
view =
    ( { title = "Scenario Not Found"
      , body =
            [ Html.main_
                [ Attr.css
                    [ Tw.flex_grow
                    ]
                ]
                [ Html.div
                    [ Attr.css
                        [ Tw.container
                        , Tw.mx_auto
                        , Tw.p_3
                        , Tw.flex
                        , Tw.flex_col
                        ]
                    ]
                    [ Html.p
                        [ Attr.css
                            [ Tw.text_center
                            , Tw.mt_4
                            , Tw.grid
                            , Tw.grid_cols_1
                            , Tw.divide_y
                            , Tw.border
                            , Tw.border_color TwTheme.blue_500
                            , Tw.rounded
                            , Tw.bg_color TwTheme.white
                            , Tw.text_color TwTheme.black
                            , Tw.border_color TwTheme.blue_200
                            , Tw.font_semibold
                            , Tw.p_2
                            ]
                        ]
                        [ Html.text "Scenario Not Found"
                        ]
                    ]
                ]
            ]
      }
    , Action.None
    )

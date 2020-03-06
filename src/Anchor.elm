module Anchor exposing (Anchor(..), string)

import ChainConnector exposing (ChainConnector)
import Connector exposing (Connector)
import Fixing exposing (Fixing)


{-| What's the anchor like?
-}
type Anchor
    = None
    | Single Fixing ChainConnector -- 1 fixing, 1 connector
    | Twin Fixing ChainConnector -- 2 fixings, 2 connectors
    | Joined Fixing Connector -- 2 fixings, 1 connector


string : Anchor -> String
string anchor =
    case anchor of
        None ->
            "the anchor has been cut off."

        Single fixing chainConnector ->
            ChainConnector.singular chainConnector ++ " attached to a " ++ Fixing.singular fixing ++ "."

        Twin fixing chainConnector ->
            "a pair of " ++ ChainConnector.plural chainConnector ++ " attached to " ++ Fixing.plural fixing ++ "."

        Joined fixing connector ->
            Connector.singular connector ++ " attached by chains to two " ++ Fixing.plural fixing ++ "."

module ChainConnector exposing (ChainConnector(..), plural, singular)

import Connector exposing (Connector)


type ChainConnector
    = NoChain Connector
    | Chain Connector


singular : ChainConnector -> String
singular chain =
    case chain of
        NoChain connector ->
            Connector.singular connector

        Chain connector ->
            "a chain with " ++ Connector.singular connector


plural : ChainConnector -> String
plural chain =
    case chain of
        NoChain connector ->
            Connector.plural connector

        Chain connector ->
            "chains with " ++ Connector.singular connector ++ " on each"

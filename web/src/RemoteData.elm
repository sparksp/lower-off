module RemoteData exposing (RemoteData(..))


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a

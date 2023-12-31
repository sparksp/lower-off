module Main exposing (Model, Msg, main)

import Action exposing (Action)
import Anchor exposing (Anchor)
import Anchor.API
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Browser.Styled as Browser exposing (Document)
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Http
import Page exposing (Page)
import Page.Blank as Blank
import Page.Gallery as Gallery
import Page.Menu as Home
import Page.NotFound as NotFound
import Page.Scenario as Scenario
import Route exposing (Route)
import Session exposing (Session)
import Tailwind.Utilities as Tw
import Url exposing (Url)


type Model
    = Model (List Anchor) Section


type Section
    = Loading Session (Maybe Route)
    | NotFound Session
    | Error Session
    | Menu Session
    | Gallery Session Int
    | Scenario Scenario.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    ( Model [] (Loading (Session.session navKey) (Route.fromUrl url))
    , Anchor.API.fetch "/api" GotAnchors
    )


view : Model -> Document Msg
view (Model anchors section) =
    case section of
        Loading _ _ ->
            Page.view Page.Menu Blank.view

        NotFound _ ->
            Page.view Page.Other NotFound.view

        Error _ ->
            Page.view Page.Other NotFound.view

        Menu _ ->
            Page.view Page.Menu Home.view
                |> preloadAnchorImages anchors

        Gallery _ id ->
            Page.view Page.Gallery (Gallery.view anchors id)

        Scenario scenario ->
            viewPage Page.Scenario GotScenarioMsg (Scenario.view scenario)
                |> preloadAnchorImages anchors


viewPage : Page -> (msgA -> msgB) -> ( Document msgA, Action msgA ) -> Document msgB
viewPage page toMsg doc =
    let
        { title, body } =
            Page.view page doc
    in
    { title = title
    , body = List.map (Html.map toMsg) body
    }


preloadAnchorImages : List Anchor -> Document Msg -> Document Msg
preloadAnchorImages anchors { title, body } =
    { title = title
    , body = body ++ [ Html.div [ Attr.css [ Tw.hidden ] ] (List.map Anchor.toHtml anchors) ]
    }


type Msg
    = ClickedLink UrlRequest
    | ChangedUrl Url
    | GotAnchors (Result Http.Error (List Anchor))
    | GotScenarioMsg Scenario.Msg


toSession : Model -> Session
toSession (Model _ section) =
    case section of
        Loading session _ ->
            session

        NotFound session ->
            session

        Error session ->
            session

        Menu session ->
            session

        Gallery session _ ->
            session

        Scenario scenario ->
            Scenario.toSession scenario


changeRouteTo : Maybe Route -> List Anchor -> Session -> ( Section, Cmd Msg )
changeRouteTo maybeRoute anchors session =
    case maybeRoute of
        Nothing ->
            ( NotFound session
            , Cmd.none
            )

        Just Route.Home ->
            ( Menu session
            , Cmd.none
            )

        Just Route.Gallery ->
            ( Gallery session 1
            , Session.replaceUrl session (Route.GalleryItem 1 |> Route.toUrl)
            )

        Just (Route.GalleryItem id) ->
            ( Gallery session id
            , Cmd.none
            )

        Just Route.Scenario ->
            Scenario.init session anchors
                |> updateWith Scenario GotScenarioMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model anchors section) as model) =
    case ( msg, section ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Session.pushUrl (toSession model) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) anchors (toSession model)
                |> Tuple.mapFirst (Model anchors)

        ( GotAnchors (Ok gotAnchors), Loading session maybeRoute ) ->
            changeRouteTo maybeRoute gotAnchors session
                |> Tuple.mapFirst (Model gotAnchors)

        ( GotAnchors (Ok gotAnchors), _ ) ->
            ( Model gotAnchors section
            , Cmd.none
            )

        ( GotAnchors (Err _), _ ) ->
            ( Model anchors (Error (toSession model))
            , Cmd.none
            )

        ( GotScenarioMsg scenarioMsg, Scenario scenario ) ->
            Scenario.update scenarioMsg scenario
                |> updateWith Scenario GotScenarioMsg
                |> Tuple.mapFirst (Model anchors)

        ( GotScenarioMsg _, _ ) ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view >> Browser.toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }


updateWith : (subModel -> Section) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Section, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )

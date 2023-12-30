module Main exposing (Model, Msg, main)

import Action exposing (Action)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Browser.Styled as Browser exposing (Document)
import Html.Styled as Html
import Page exposing (Page)
import Page.Blank as Blank
import Page.Gallery as Gallery
import Page.Menu as Home
import Page.NotFound as NotFound
import Page.Scenario as Scenario
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)


type Model
    = Redirect Session
    | NotFound Session
    | Menu Session
    | Gallery Gallery.Model
    | Scenario Scenario.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo (Route.fromUrl url) (Redirect (Session.session navKey))


view : Model -> Document Msg
view model =
    case model of
        Redirect _ ->
            Page.view Page.Other Blank.view

        NotFound _ ->
            Page.view Page.Other NotFound.view

        Menu _ ->
            Page.view Page.Menu Home.view

        Gallery gallery ->
            viewPage Page.Gallery GotGalleryMsg (Gallery.view gallery)

        Scenario scenario ->
            viewPage Page.Scenario GotScenarioMsg (Scenario.view scenario)


viewPage : Page -> (msgA -> msgB) -> ( Document msgA, Action msgA ) -> Document msgB
viewPage page toMsg doc =
    let
        { title, body } =
            Page.view page doc
    in
    { title = title
    , body = List.map (Html.map toMsg) body
    }


type Msg
    = ClickedLink UrlRequest
    | ChangedUrl Url
    | GotGalleryMsg Gallery.Msg
    | GotScenarioMsg Scenario.Msg


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Menu session ->
            session

        Gallery gallery ->
            Gallery.toSession gallery

        Scenario scenario ->
            Scenario.toSession scenario


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session : Session
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            ( Menu session, Cmd.none )

        Just Route.Gallery ->
            Gallery.init session 1
                |> updateWith Gallery GotGalleryMsg

        Just (Route.GalleryItem id) ->
            Gallery.init session id
                |> updateWith Gallery GotGalleryMsg

        Just Route.Scenario ->
            Scenario.init session
                |> updateWith Scenario GotScenarioMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
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
            changeRouteTo (Route.fromUrl url) model

        ( GotGalleryMsg galleryMsg, Gallery gallery ) ->
            Gallery.update galleryMsg gallery
                |> updateWith Gallery GotGalleryMsg

        ( GotGalleryMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotScenarioMsg scenarioMsg, Scenario scenario ) ->
            Scenario.update scenarioMsg scenario
                |> updateWith Scenario GotScenarioMsg

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


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )

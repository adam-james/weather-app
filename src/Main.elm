module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html
import Page
import Page.About as AboutPage
import Page.City as CityPage
import Page.Home as HomePage
import Page.NotFound as NotFoundPage
import Url
import Url.Parser as Parser exposing ((</>))



-- ROUTES


type Route
    = Home
    | About
    | City


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Home (Parser.s "home")
        , Parser.map City (Parser.s "city")
        , Parser.map City (Parser.s "city" </> Parser.s "current-weather")
        , Parser.map City (Parser.s "city" </> Parser.s "forecast")
        , Parser.map About (Parser.s "about")
        ]


matchRoute : Url.Url -> Maybe Route
matchRoute url =
    Parser.parse routeParser url



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , homeModel : HomePage.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url HomePage.init, Cmd.none )



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg HomePage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        GotHomeMsg homeMsg ->
            let
                homeModel =
                    HomePage.update homeMsg model.homeModel
            in
            ( { model | homeModel = homeModel }, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case matchRoute model.url of
        Just Home ->
            let
                { title, content } =
                    HomePage.view model.homeModel

                mappedContent =
                    Html.map GotHomeMsg content
            in
            Page.view { title = title, content = mappedContent }

        Just City ->
            Page.view (CityPage.view model.url)

        Just About ->
            Page.view AboutPage.view

        Nothing ->
            Page.view NotFoundPage.view



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

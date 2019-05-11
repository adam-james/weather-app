module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attrs exposing (checked, class, for, href, name, type_)
import Html.Events as Evnts
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
    | City Int


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Home (Parser.s "home")
        , Parser.map City (Parser.s "city" </> Parser.int)
        , Parser.map City (Parser.s "city" </> Parser.int </> Parser.s "current-weather")
        , Parser.map City (Parser.s "city" </> Parser.int </> Parser.s "forecast")
        , Parser.map About (Parser.s "about")
        ]


matchRoute : Url.Url -> Maybe Route
matchRoute url =
    Parser.parse routeParser url



---- MODEL ----


type SettingsModalState
    = Open
    | Closed


type TemperatureScale
    = Farhenheit
    | Celsius


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , homeModel : HomePage.Model
    , cityModel : CityPage.Model
    , settingsModalState : SettingsModalState
    , tempScale : TemperatureScale
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( homeModel, _ ) =
            HomePage.init key

        ( cityModel, cityCmd ) =
            case matchRoute url of
                Just (City cityId) ->
                    CityPage.init (Just cityId)

                _ ->
                    CityPage.init Nothing
    in
    ( Model
        key
        url
        homeModel
        cityModel
        Closed
        Farhenheit
    , Cmd.map GotCityMsg cityCmd
    )



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg HomePage.Msg
    | GotCityMsg CityPage.Msg
    | OpenSettingsModal
    | CloseSettingsModal
    | SetTempScale String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTempScale id ->
            let
                tempScale =
                    tempScaleFromId id
            in
            case tempScale of
                Just scale ->
                    -- TODO perform conversions here
                    ( { model | tempScale = scale }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OpenSettingsModal ->
            ( { model | settingsModalState = Open }, Cmd.none )

        CloseSettingsModal ->
            ( { model | settingsModalState = Closed }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                ( cityModel, cityCmd ) =
                    case matchRoute url of
                        Just (City cityId) ->
                            CityPage.init (Just cityId)

                        _ ->
                            CityPage.init Nothing
            in
            case matchRoute url of
                Just (City _) ->
                    ( { model
                        | url = url
                        , cityModel = cityModel
                      }
                    , Cmd.map GotCityMsg cityCmd
                    )

                _ ->
                    ( { model
                        | url = url
                      }
                    , Cmd.none
                    )

        GotHomeMsg homeMsg ->
            let
                ( homeModel, homeCmd ) =
                    HomePage.update homeMsg model.homeModel
            in
            ( { model | homeModel = homeModel }, Cmd.map GotHomeMsg homeCmd )

        GotCityMsg cityMsg ->
            let
                ( cityModel, cityCmd ) =
                    CityPage.update cityMsg model.cityModel
            in
            ( { model | cityModel = cityModel }, Cmd.map GotCityMsg cityCmd )



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
            pageView model { title = title, content = mappedContent }

        Just (City _) ->
            pageView model (CityPage.view model.cityModel)

        Just About ->
            pageView model AboutPage.view

        Nothing ->
            pageView model NotFoundPage.view


pageView : Model -> { title : String, content : Html Msg } -> Browser.Document Msg
pageView model page =
    let
        body =
            case model.settingsModalState of
                Open ->
                    [ pageHeader model
                    , page.content
                    , settingsModal model
                    ]

                Closed ->
                    [ pageHeader model
                    , page.content
                    ]
    in
    { title = "Weather App - " ++ page.title
    , body = body
    }


pageHeader : Model -> Html Msg
pageHeader model =
    let
        link =
            case model.cityModel.forecast of
                CityPage.Success { city } ->
                    [ a [ href "/", class "page-header__nav-link" ]
                        [ text (city.name ++ ", " ++ city.country) ]
                    ]

                _ ->
                    [ span [] [] ]
    in
    header [ class "page-header" ]
        [ ul [ class "page-header__nav" ]
            [ li [ class "page-header__nav-item" ] link
            ]
        , button
            [ class "open-settings"
            , Evnts.onClick OpenSettingsModal
            ]
            [ text (tempScaleText model.tempScale) ]
        ]


settingsModal : Model -> Html Msg
settingsModal model =
    div [ class "settings-modal" ]
        [ header [ class "settings-modal__header" ]
            [ h2 [ class "settings-modal__title" ] [ text "Settings" ]
            , button
                [ Evnts.onClick CloseSettingsModal
                , class "settings-modal__close-button"
                ]
                [ text "Close" ]
            ]
        , h3 [ class "settings-modal__content-title" ]
            [ text "Your Settings" ]
        , form [ class "settings-modal__form" ]
            [ p []
                [ text "Temperature Scale" ]
            , div []
                [ tempScaleInput Farhenheit model
                , tempScaleInput Celsius model
                ]
            ]
        ]


tempScaleInput : TemperatureScale -> Model -> Html Msg
tempScaleInput tempScale model =
    let
        baseLabelClass =
            "settings-modal__radio-label"

        checked =
            tempScale == model.tempScale

        labelClass =
            if checked then
                baseLabelClass ++ " settings-modal__radio-label--checked"

            else
                baseLabelClass
    in
    div [ class "settings-modal__radio-container" ]
        [ label
            [ for (tempScaleId tempScale)
            , class labelClass
            ]
            [ text (tempScaleText tempScale) ]
        , input
            [ type_ "radio"
            , name "temp-scale"
            , Attrs.id (tempScaleId tempScale)
            , class "settings-modal__radio-input"
            , Attrs.checked checked
            , Attrs.value (tempScaleId tempScale)
            , Evnts.onInput SetTempScale
            ]
            []
        ]



-- TempartureScale


tempScaleId : TemperatureScale -> String
tempScaleId tempScale =
    case tempScale of
        Farhenheit ->
            "fahrenheit"

        Celsius ->
            "celsius"


tempScaleFromId : String -> Maybe TemperatureScale
tempScaleFromId str =
    case str of
        "fahrenheit" ->
            Just Farhenheit

        "celsius" ->
            Just Celsius

        _ ->
            Nothing


tempScaleText : TemperatureScale -> String
tempScaleText tempScale =
    case tempScale of
        Farhenheit ->
            "F°"

        Celsius ->
            "C°"



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

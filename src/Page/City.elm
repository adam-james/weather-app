module Page.City exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode
import Page.City.Forecast as ForecastPage
import Url
import Url.Parser as Parser exposing ((</>))



-- MODEL
-- TODO add NotAsked


type Request a
    = Loading
    | Success a
    | Failure


type alias Main =
    { temp : Float
    , pressure : Int
    , humidity : Int
    , temp_min : Float
    , temp_max : Float
    }


type alias CurrentWeather =
    { main : Main }


type alias Model =
    { currentWeather : Request CurrentWeather }


init : Maybe Int -> ( Model, Cmd Msg )
init maybeId =
    case maybeId of
        Nothing ->
            ( { currentWeather = Loading }, Cmd.none )

        Just id ->
            ( { currentWeather = Loading }, getCurrentWeather id )



-- UPDATE


type Msg
    = NoOp
    | GotCurrentWeather (Result Http.Error CurrentWeather)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotCurrentWeather result ->
            case result of
                Err _ ->
                    ( { model | currentWeather = Failure }
                    , Cmd.none
                    )

                Ok currentWeather ->
                    ( { model | currentWeather = Success currentWeather }
                    , Cmd.none
                    )



-- ROUTING


type Route
    = CurrentWeatherRoute Int
    | Forecast Int


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map CurrentWeatherRoute (Parser.s "city" </> Parser.int)
        , Parser.map CurrentWeatherRoute
            (Parser.s "city" </> Parser.int </> Parser.s "current-weather")
        , Parser.map Forecast
            (Parser.s "city" </> Parser.int </> Parser.s "forecast")
        ]


matchRoute : Url.Url -> Maybe Route
matchRoute url =
    Parser.parse routeParser url



-- VIEW


view : Url.Url -> Model -> { title : String, content : Html msg }
view url model =
    case matchRoute url of
        Just (CurrentWeatherRoute cityId) ->
            baseView cityId (currentWeatherView model)

        Just (Forecast cityId) ->
            baseView cityId ForecastPage.view

        Nothing ->
            { title = "City - Not Found"
            , content = h1 [] [ text "Not Found" ]
            }


currentWeatherView : Model -> Html mgs
currentWeatherView model =
    case model.currentWeather of
        Loading ->
            p [] [ text "Loading..." ]

        Failure ->
            p [] [ text "Something went wrong :(" ]

        Success currentWeather ->
            p [] [ text ("The temperature is " ++ String.fromFloat currentWeather.main.temp) ]


baseView : Int -> Html msg -> { title : String, content : Html msg }
baseView cityId subView =
    { title = "City"
    , content =
        div []
            [ ul
                []
                [ li [] [ a [ href (currentWeatherHref cityId) ] [ text "Current Weather" ] ]
                , li [] [ a [ href (forecastHref cityId) ] [ text "Forecast" ] ]
                ]
            , h2
                []
                [ text ("City Id: " ++ String.fromInt cityId) ]
            , subView
            ]
    }


currentWeatherHref : Int -> String
currentWeatherHref cityId =
    "/city/" ++ String.fromInt cityId ++ "/current-weather"


forecastHref : Int -> String
forecastHref cityId =
    "/city/" ++ String.fromInt cityId ++ "/forecast"



-- HTTP


getCurrentWeather : Int -> Cmd Msg
getCurrentWeather cityId =
    Http.get
        { url = "http://localhost:5000/current-weather?cityId=" ++ String.fromInt cityId
        , expect = Http.expectJson GotCurrentWeather currentWeatherDecoder
        }


currentWeatherDecoder : Decode.Decoder CurrentWeather
currentWeatherDecoder =
    Decode.map CurrentWeather
        (Decode.field "main" mainDecoder)


mainDecoder : Decode.Decoder Main
mainDecoder =
    Decode.map5 Main
        (Decode.field "temp" Decode.float)
        (Decode.field "pressure" Decode.int)
        (Decode.field "humidity" Decode.int)
        (Decode.field "temp_min" Decode.float)
        (Decode.field "temp_max" Decode.float)

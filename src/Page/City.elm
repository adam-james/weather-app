module Page.City exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode
import Page.City.Forecast as ForecastPage
import Url
import Url.Parser as Parser exposing ((</>))



-- MODEL


type Request a
    = Loading
    | Success a
    | Failure


type alias Main =
    { temp : Float
    , pressure : Int
    , humidity : Int
    , tempMin : Float
    , tempMax : Float
    }


type alias Coord =
    { lon : Float
    , lat : Float
    }


type alias Wind =
    { speed : Float
    , degrees : Int
    }


type alias Sys =
    { country : String
    , sunrise : Int
    , sunset : Int
    }


type alias CurrentWeather =
    { main : Main
    , coor : Coord
    , wind : Wind
    , sys : Sys
    , name : String
    }


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


currentWeatherView : Model -> Html msg
currentWeatherView model =
    case model.currentWeather of
        Loading ->
            p [] [ text "Loading..." ]

        Failure ->
            p [] [ text "Something went wrong :(" ]

        Success currentWeather ->
            tmpWeatherBreakdown currentWeather


tmpWeatherBreakdown : CurrentWeather -> Html msg
tmpWeatherBreakdown currentWeather =
    section []
        [ h2 [] [ text (cityName currentWeather) ]
        , p [] [ text ("Latitude: " ++ String.fromFloat currentWeather.coor.lat) ]
        , p [] [ text ("Longitude: " ++ String.fromFloat currentWeather.coor.lon) ]
        , p [] [ text ("Humidity: " ++ String.fromInt currentWeather.main.humidity) ]
        , p [] [ text ("Pressure: " ++ String.fromInt currentWeather.main.pressure) ]
        , p [] [ text ("Temperature (F): " ++ String.fromFloat currentWeather.main.temp) ]
        , p [] [ text ("High (F): " ++ String.fromFloat currentWeather.main.tempMax) ]
        , p [] [ text ("Low (F): " ++ String.fromFloat currentWeather.main.tempMin) ]
        , p [] [ text ("Sunrise: " ++ String.fromInt currentWeather.sys.sunrise) ]
        , p [] [ text ("Sunset: " ++ String.fromInt currentWeather.sys.sunset) ]
        , p [] [ text ("Wind Speed: " ++ String.fromFloat currentWeather.wind.speed) ]
        , p [] [ text ("Wind Direction: " ++ String.fromInt currentWeather.wind.degrees) ]
        ]


cityName : CurrentWeather -> String
cityName currentWeather =
    currentWeather.name ++ ", " ++ currentWeather.sys.country


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
    Decode.map5 CurrentWeather
        (Decode.field "main" mainDecoder)
        (Decode.field "coord" coordDecoder)
        (Decode.field "wind" windDecoder)
        (Decode.field "sys" sysDecoder)
        (Decode.field "name" Decode.string)


sysDecoder : Decode.Decoder Sys
sysDecoder =
    Decode.map3 Sys
        (Decode.field "country" Decode.string)
        (Decode.field "sunrise" Decode.int)
        (Decode.field "sunset" Decode.int)


windDecoder : Decode.Decoder Wind
windDecoder =
    Decode.map2 Wind
        (Decode.field "speed" Decode.float)
        (Decode.field "deg" Decode.int)


coordDecoder : Decode.Decoder Coord
coordDecoder =
    Decode.map2 Coord
        (Decode.field "lon" Decode.float)
        (Decode.field "lat" Decode.float)


mainDecoder : Decode.Decoder Main
mainDecoder =
    Decode.map5 Main
        (Decode.field "temp" Decode.float)
        (Decode.field "pressure" Decode.int)
        (Decode.field "humidity" Decode.int)
        (Decode.field "temp_min" Decode.float)
        (Decode.field "temp_max" Decode.float)

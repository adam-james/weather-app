module Page.City exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (class, href)
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


type alias Wind =
    { speed : Float
    , degrees : Int
    }


type alias Clouds =
    { all : Int
    }


type alias Sys =
    { country : String
    , sunrise : Int
    , sunset : Int
    }



-- TODO what is this list of Weather?


type alias Weather =
    { id : Int
    , main : String
    , description : String
    , icon : String
    }



-- TODO There is also rain and snow in this, but it seems optional.


type alias CurrentWeather =
    { main : Main
    , wind : Wind
    , sys : Sys
    , name : String
    , weather : List Weather
    , visibility : Int
    , datetime : Int
    , clouds : Clouds
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


view : Model -> { title : String, content : Html msg }
view model =
    baseView (currentWeatherView model)


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
        , p [] [ text ("Time : " ++ String.fromInt currentWeather.datetime) ]
        , h3 [] [ text "Current Weather" ]
        , firstWeather currentWeather.weather
        , p [] [ text ("Temperature (F): " ++ String.fromFloat currentWeather.main.temp) ]
        , p [] [ text ("High (F): " ++ String.fromFloat currentWeather.main.tempMax) ]
        , p [] [ text ("Low (F): " ++ String.fromFloat currentWeather.main.tempMin) ]
        , p [] [ text ("Cloudiness (%): " ++ String.fromInt currentWeather.clouds.all) ]
        , p [] [ text ("Humidity: " ++ String.fromInt currentWeather.main.humidity) ]
        , p [] [ text ("Pressure: " ++ String.fromInt currentWeather.main.pressure) ]
        , h3 [] [ text "Sunrise - Sunset Section" ]
        , p [] [ text ("Sunrise: " ++ String.fromInt currentWeather.sys.sunrise) ]
        , p [] [ text ("Sunset: " ++ String.fromInt currentWeather.sys.sunset) ]
        , h3 [] [ text "Wind Section" ]
        , p [] [ text ("Wind Speed: " ++ String.fromFloat currentWeather.wind.speed) ]
        , p [] [ text ("Wind Direction: " ++ String.fromInt currentWeather.wind.degrees) ]
        ]


firstWeather : List Weather -> Html msg
firstWeather weathers =
    let
        first =
            List.head weathers

        outer children =
            section [ class "first-weather" ]
                (h3 [] [ text "Icon stuff" ]
                    :: children
                )
    in
    case first of
        Nothing ->
            outer
                [ p
                    []
                    [ text "No first weather!" ]
                ]

        Just weather ->
            outer
                [ p [] [ text ("Id: " ++ String.fromInt weather.id) ]
                , p [] [ text ("Description: " ++ weather.description) ]
                , p [] [ text ("Icon: " ++ weather.icon) ]
                , p [] [ text ("Main: " ++ weather.main) ]
                ]


cityName : CurrentWeather -> String
cityName currentWeather =
    currentWeather.name ++ ", " ++ currentWeather.sys.country


baseView : Html msg -> { title : String, content : Html msg }
baseView subView =
    { title = "City"
    , content = subView
    }



-- HTTP


getCurrentWeather : Int -> Cmd Msg
getCurrentWeather cityId =
    Http.get
        { url = "http://localhost:5000/current-weather?cityId=" ++ String.fromInt cityId
        , expect = Http.expectJson GotCurrentWeather currentWeatherDecoder
        }


currentWeatherDecoder : Decode.Decoder CurrentWeather
currentWeatherDecoder =
    Decode.map8 CurrentWeather
        (Decode.field "main" mainDecoder)
        (Decode.field "wind" windDecoder)
        (Decode.field "sys" sysDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "weather" weathersDecoder)
        (Decode.field "visibility" Decode.int)
        (Decode.field "dt" Decode.int)
        (Decode.field "clouds" cloudsDecoder)


cloudsDecoder : Decode.Decoder Clouds
cloudsDecoder =
    Decode.map Clouds
        (Decode.field "all" Decode.int)


weathersDecoder : Decode.Decoder (List Weather)
weathersDecoder =
    Decode.list weatherDecoder


weatherDecoder : Decode.Decoder Weather
weatherDecoder =
    Decode.map4 Weather
        (Decode.field "id" Decode.int)
        (Decode.field "main" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "icon" Decode.string)


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


mainDecoder : Decode.Decoder Main
mainDecoder =
    Decode.map5 Main
        (Decode.field "temp" Decode.float)
        (Decode.field "pressure" Decode.int)
        (Decode.field "humidity" Decode.int)
        (Decode.field "temp_min" Decode.float)
        (Decode.field "temp_max" Decode.float)

module Page.City exposing
    ( Model
    , Msg
    , Request(..)
    , init
    , update
    , view
    )

import City exposing (City, cityDecoder)
import DisplayTime exposing (displayDateTime)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as Decode
import Task
import Time
import Url
import Url.Parser as Parser exposing ((</>))
import WeatherIcon



-- MODEL


type Request a
    = Loading
    | Success a
    | Failure Http.Error


type alias Main =
    { temp : Float
    , pressure : Float
    , humidity : Float
    , tempMin : Float
    , tempMax : Float
    }


type alias Wind =
    { speed : Float
    , degrees : Float
    }


type alias Clouds =
    { all : Int
    }


type alias Sys =
    { country : String
    , sunrise : Int
    , sunset : Int
    }


type alias Weather =
    { id : Int
    , main : String
    , description : String
    , icon : WeatherIcon.WeatherIcon
    }


type alias CurrentWeather =
    { main : Main
    , wind : Wind
    , sys : Sys
    , name : String
    , weather : List Weather
    , datetime : Int
    , clouds : Clouds
    }


type alias ForecastItem =
    { datetime : Int
    , weather : List Weather
    , main : Main
    , wind : Wind
    , clouds : Clouds
    }


type alias Forecast =
    { city : City
    , items : List ForecastItem
    }


type alias Model =
    { currentWeather : Request CurrentWeather
    , timezone : Time.Zone
    , forecast : Request Forecast
    }


init : Maybe Int -> ( Model, Cmd Msg )
init maybeId =
    case maybeId of
        Nothing ->
            ( { currentWeather = Loading
              , timezone = Time.utc
              , forecast = Loading
              }
            , Cmd.none
            )

        Just id ->
            ( { currentWeather = Loading
              , timezone = Time.utc
              , forecast = Loading
              }
            , Cmd.batch
                [ Task.perform SetTimezone Time.here
                , getCurrentWeather id
                , getForecast id
                ]
            )



-- UPDATE


type Msg
    = NoOp
    | GotCurrentWeather (Result Http.Error CurrentWeather)
    | GotForecast (Result Http.Error Forecast)
    | SetTimezone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotCurrentWeather result ->
            case result of
                Err error ->
                    ( { model | currentWeather = Failure error }
                    , Cmd.none
                    )

                Ok currentWeather ->
                    ( { model | currentWeather = Success currentWeather }
                    , Cmd.none
                    )

        GotForecast result ->
            case result of
                Err error ->
                    ( { model | forecast = Failure error }
                    , Cmd.none
                    )

                Ok forecast ->
                    ( { model | forecast = Success forecast }
                    , Cmd.none
                    )

        SetTimezone timezone ->
            ( { model | timezone = timezone }, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html msg }
view model =
    baseView
        (div
            []
            [ requestView model.forecast (tmpForecastView model.timezone)
            ]
        )


requestView : Request a -> (a -> Html msg) -> Html msg
requestView request render =
    case request of
        Loading ->
            p [] [ text "Loading..." ]

        Failure _ ->
            p [] [ text "Something went wrong :(" ]

        Success data ->
            render data


tmpForecastView : Time.Zone -> Forecast -> Html msg
tmpForecastView timezone forecast =
    section []
        [ h2 [ class "page-title" ] [ text "5 Day Forecast" ]
        , ul [ class "forecast-list" ] (List.map (tmpForecastItem timezone) forecast.items)
        ]


tmpForecastItem : Time.Zone -> ForecastItem -> Html msg
tmpForecastItem timezone item =
    li [ class "forecast-list__item" ]
        [ h3 [ class "forecast__date" ] [ text (DisplayTime.displayDate timezone item.datetime) ]
        , forecastInfo item
        ]


forecastInfo : ForecastItem -> Html msg
forecastInfo item =
    let
        first =
            List.head item.weather

        outer children =
            section [ class "forecast__info" ] children
    in
    case first of
        Nothing ->
            outer
                [ section
                    []
                    [ text "No weather icon!" ]
                ]

        Just weather ->
            outer
                [ div [ class "forecast__icon-container" ] [ WeatherIcon.mapIcon weather.icon ]
                , div [ class "forecast__temps" ]
                    [ p [ class "forecast__current-temp" ] [ text (String.fromInt (round item.main.temp) ++ " F°") ]
                    , p [ class "forecast__high-low" ]
                        [ text (highLow item.main) ]
                    ]
                ]


highLow : Main -> String
highLow itemMain =
    (String.fromInt (round itemMain.tempMin) ++ " F°")
        ++ " | "
        ++ (String.fromInt (round itemMain.tempMax) ++ " F°")


currentWeatherView : Model -> Html msg
currentWeatherView model =
    case model.currentWeather of
        Loading ->
            p [] [ text "Loading..." ]

        Failure _ ->
            p [] [ text "Something went wrong :(" ]

        Success currentWeather ->
            tmpWeatherBreakdown model.timezone currentWeather


tmpWeatherBreakdown : Time.Zone -> CurrentWeather -> Html msg
tmpWeatherBreakdown timezone currentWeather =
    section []
        [ h2 [] [ text (cityName currentWeather) ]
        , h3 [] [ text "Current Weather" ]
        , p [] [ text ("Temperature (F): " ++ String.fromFloat currentWeather.main.temp) ]
        , p [] [ text ("High (F): " ++ String.fromFloat currentWeather.main.tempMax) ]
        , p [] [ text ("Low (F): " ++ String.fromFloat currentWeather.main.tempMin) ]
        , h3 [] [ text "Wind" ]
        , p [] [ text ("Wind Speed: " ++ String.fromFloat currentWeather.wind.speed) ]
        , p [] [ text ("Wind Direction: " ++ String.fromFloat currentWeather.wind.degrees) ]
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


getForecast : Int -> Cmd Msg
getForecast cityId =
    Http.get
        { url = "http://localhost:5000/forecast?cityId=" ++ String.fromInt cityId
        , expect = Http.expectJson GotForecast forecastDecoder
        }


forecastDecoder : Decode.Decoder Forecast
forecastDecoder =
    Decode.map2 Forecast
        (Decode.field "city" cityDecoder)
        (Decode.field "list" forecastItemsDecoder)


forecastItemsDecoder : Decode.Decoder (List ForecastItem)
forecastItemsDecoder =
    Decode.list forecastItemDecoder


forecastItemDecoder : Decode.Decoder ForecastItem
forecastItemDecoder =
    Decode.map5 ForecastItem
        (Decode.field "dt" Decode.int)
        (Decode.field "weather" weathersDecoder)
        (Decode.field "main" mainDecoder)
        (Decode.field "wind" windDecoder)
        (Decode.field "clouds" cloudsDecoder)


currentWeatherDecoder : Decode.Decoder CurrentWeather
currentWeatherDecoder =
    Decode.map7 CurrentWeather
        (Decode.field "main" mainDecoder)
        (Decode.field "wind" windDecoder)
        (Decode.field "sys" sysDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "weather" weathersDecoder)
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
        (Decode.field "icon" WeatherIcon.decode)


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
        (Decode.field "deg" Decode.float)


mainDecoder : Decode.Decoder Main
mainDecoder =
    Decode.map5 Main
        (Decode.field "temp" Decode.float)
        (Decode.field "pressure" Decode.float)
        (Decode.field "humidity" Decode.float)
        (Decode.field "temp_min" Decode.float)
        (Decode.field "temp_max" Decode.float)

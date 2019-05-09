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
import Forecast
    exposing
        ( Forecast
        , ForecastItem
        , Main
        , forecastDecoder
        )
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


type alias Model =
    { timezone : Time.Zone
    , forecast : Request Forecast
    }


init : Maybe Int -> ( Model, Cmd Msg )
init maybeId =
    case maybeId of
        Nothing ->
            ( { timezone = Time.utc
              , forecast = Loading
              }
            , Cmd.none
            )

        Just id ->
            ( { timezone = Time.utc
              , forecast = Loading
              }
            , Cmd.batch
                [ Task.perform SetTimezone Time.here
                , getForecast id
                ]
            )



-- UPDATE


type Msg
    = NoOp
    | GotForecast (Result Http.Error Forecast)
    | SetTimezone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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



-- TODO group forecast items by day


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
            List.head item.weathers

        outer children =
            section [ class "forecast__info" ] children
    in
    case first of
        Nothing ->
            outer
                [ section
                    []
                    [ text "No data" ]
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


baseView : Html msg -> { title : String, content : Html msg }
baseView subView =
    { title = "City"
    , content = subView
    }



-- HTTP


getForecast : Int -> Cmd Msg
getForecast cityId =
    Http.get
        { url = "http://localhost:5000/forecast?cityId=" ++ String.fromInt cityId
        , expect = Http.expectJson GotForecast forecastDecoder
        }

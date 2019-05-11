module Page.City exposing
    ( Model
    , Msg(..)
    , Request(..)
    , init
    , update
    , view
    )

import City exposing (City, cityDecoder)
import DisplayTime exposing (displayDateTime)
import Forecast
    exposing
        ( DayForecast
        , FiveDayForecast
        , Forecast
        , Main
        , Temperature
        , forecastDecoder
        )
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as Decode
import Task
import TemperatureScale as TS
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
    , forecast : Request FiveDayForecast
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
    | ConvertTemp


update : TS.TemperatureScale -> Msg -> Model -> ( Model, Cmd Msg )
update tempScale msg model =
    case msg of
        ConvertTemp ->
            case model.forecast of
                Success forecast ->
                    ( { model
                        | forecast = Success (Forecast.convertFiveDayForecast tempScale forecast)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        GotForecast result ->
            case result of
                Err error ->
                    ( { model | forecast = Failure error }
                    , Cmd.none
                    )

                Ok forecast ->
                    ( { model
                        | forecast =
                            Success
                                (Forecast.fiveDayForecast
                                    model.timezone
                                    tempScale
                                    forecast
                                )
                      }
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
            [ requestView model.forecast (forecastView model.timezone)
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


forecastView : Time.Zone -> FiveDayForecast -> Html msg
forecastView timezone forecast =
    let
        first =
            List.head forecast.items

        rest =
            List.tail forecast.items
    in
    case ( first, rest ) of
        ( Just current, Just future ) ->
            div [ class "container" ]
                [ section []
                    [ h3 [ class "section-title" ] [ text "Current Weather" ]
                    , forecastArticle timezone current (WeatherIcon.mapIcon current.summary.icon)
                    ]
                , section []
                    [ h3 [ class "section-title" ] [ text "5 Day Forecast" ]
                    , ul [ class "forecast-list" ] (List.map (forecastItem timezone) future)
                    ]
                ]

        _ ->
            h2 [] [ text "No forecast" ]


forecastArticle : Time.Zone -> DayForecast -> Html msg -> Html msg
forecastArticle timezone item icon =
    article [ class "forecast-article" ]
        [ h3 [ class "forecast__date" ]
            [ text (DisplayTime.displayDate timezone item.datetime) ]
        , section [ class "forecast__info" ]
            [ div [ class "forecast__icon-container" ] [ icon ]
            , div [ class "forecast__temps" ]
                [ p [ class "forecast__current-temp" ]
                    [ text (displayTemp item.summary.tempMean) ]
                , p [ class "forecast__high-low" ]
                    [ text (highLow item) ]
                ]
            ]
        ]


displayTemp : Temperature -> String
displayTemp ( float, scale ) =
    String.fromInt (round float) ++ " " ++ TS.unit scale


forecastItem : Time.Zone -> DayForecast -> Html msg
forecastItem timezone item =
    li [ class "forecast-list__item" ]
        [ forecastArticle timezone item (WeatherIcon.mapIconDay item.summary.icon) ]


highLow : DayForecast -> String
highLow item =
    -- TODO this diplay this better for screen reader
    displayTemp item.summary.tempMin
        ++ " | "
        ++ displayTemp item.summary.tempMax


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

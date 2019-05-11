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
        ( DayForecast
        , FiveDayForecast
        , Forecast
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
                    ( { model
                        | forecast =
                            Success
                                (Forecast.fiveDayForecast
                                    model.timezone
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
                    , forecastArticle timezone current
                    ]
                , section []
                    [ h3 [ class "section-title" ] [ text "5 Day Forecast" ]
                    , ul [ class "forecast-list" ] (List.map (forecastItem timezone) future)
                    ]
                ]

        _ ->
            h2 [] [ text "No forecast" ]


forecastArticle : Time.Zone -> DayForecast -> Html msg
forecastArticle timezone item =
    article [ class "forecast-article" ]
        [ h3 [ class "forecast__date" ] [ text (DisplayTime.displayDate timezone item.datetime) ]
        , forecastInfo item
        ]


forecastItem : Time.Zone -> DayForecast -> Html msg
forecastItem timezone item =
    li [ class "forecast-list__item" ] [ forecastArticle timezone item ]


forecastInfo : DayForecast -> Html msg
forecastInfo item =
    let
        outer children =
            section [ class "forecast__info" ] children
    in
    outer
        [ div [ class "forecast__icon-container" ] [ WeatherIcon.mapIcon item.summary.icon ]
        , div [ class "forecast__temps" ]
            [ p [ class "forecast__current-temp" ] [ text (String.fromInt (round item.summary.tempMean) ++ " F°") ]
            , p [ class "forecast__high-low" ]
                [ text (highLow item) ]
            ]
        ]


highLow : DayForecast -> String
highLow item =
    (String.fromInt (round item.summary.tempMin) ++ " F°")
        ++ " | "
        ++ (String.fromInt (round item.summary.tempMax) ++ " F°")


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

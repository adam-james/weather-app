module WeatherIcon exposing (mapIcon)

import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as Decode


icon : String -> Html msg
icon className =
    i [ class className, class "weather-icon" ] []


mapIcon : String -> Html msg
mapIcon weatherIcon =
    case weatherIcon of
        "01d" ->
            icon "clear-sky-day"

        "01n" ->
            icon "clear-sky-night"

        "02d" ->
            icon "few-clouds-day"

        "02n" ->
            icon "few-clouds-night"

        "03d" ->
            icon "scattered-clouds"

        "03n" ->
            icon "scattered-clouds"

        "04d" ->
            icon "broken-clouds"

        "04n" ->
            icon "broken-clouds"

        "09d" ->
            icon "shower-rain-day"

        "09n" ->
            icon "shower-rain-night"

        "10d" ->
            icon "rain-day"

        "10n" ->
            icon "rain-night"

        "11d" ->
            icon "thunderstorm-day"

        "11n" ->
            icon "thunderstorm-night"

        "13d" ->
            icon "snow-day"

        "13n" ->
            icon "snow-night"

        "50d" ->
            icon "mist-day"

        "50n" ->
            icon "mist-night"

        _ ->
            span [] [ text "No icon" ]

module WeatherIcon exposing (WeatherIcon(..), decode, mapIcon)

import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as Decode


type WeatherIcon
    = ClearSkyDay
    | ClearSkyNight
    | FewCloudsDay
    | FewCloudsNight
    | ScatteredCloudsDay
    | ScatteredCloudsNight
    | BrokenCloudsDay
    | BrokenCloudsNight
    | ShowerRainDay
    | ShowerRainNight
    | RainDay
    | RainNight
    | ThunderstormDay
    | ThunderstormNight
    | SnowDay
    | SnowNight
    | MistDay
    | MistNight
    | NoIcon


decode : Decode.Decoder WeatherIcon
decode =
    let
        convert : String -> Decode.Decoder WeatherIcon
        convert raw =
            Decode.succeed (fromString raw)
    in
    Decode.string |> Decode.andThen convert


fromString : String -> WeatherIcon
fromString str =
    case str of
        "01d" ->
            ClearSkyDay

        "01n" ->
            ClearSkyNight

        "02d" ->
            FewCloudsDay

        "02n" ->
            FewCloudsNight

        "03d" ->
            ScatteredCloudsDay

        "03n" ->
            ScatteredCloudsNight

        "04d" ->
            BrokenCloudsDay

        "04n" ->
            BrokenCloudsNight

        "09d" ->
            ShowerRainDay

        "09n" ->
            ShowerRainNight

        "10d" ->
            RainDay

        "10n" ->
            RainNight

        "11d" ->
            ThunderstormDay

        "11n" ->
            ThunderstormNight

        "13d" ->
            SnowDay

        "13n" ->
            SnowNight

        "50d" ->
            MistDay

        "50n" ->
            MistNight

        _ ->
            NoIcon


icon : String -> Html msg
icon className =
    i [ class className, class "weather-icon" ] []


mapIcon : WeatherIcon -> Html msg
mapIcon weatherIcon =
    case weatherIcon of
        ClearSkyDay ->
            icon "clear-sky-day"

        ClearSkyNight ->
            icon "clear-sky-night"

        FewCloudsDay ->
            icon "few-clouds-day"

        FewCloudsNight ->
            icon "few-clouds-night"

        ScatteredCloudsDay ->
            icon "scattered-clouds"

        ScatteredCloudsNight ->
            icon "scattered-clouds"

        BrokenCloudsDay ->
            icon "broken-clouds"

        BrokenCloudsNight ->
            icon "broken-clouds"

        ShowerRainDay ->
            icon "shower-rain-day"

        ShowerRainNight ->
            icon "shower-rain-night"

        RainDay ->
            icon "rain-day"

        RainNight ->
            icon "rain-night"

        ThunderstormDay ->
            icon "thunderstorm-day"

        ThunderstormNight ->
            icon "thunderstorm-night"

        SnowDay ->
            icon "snow-day"

        SnowNight ->
            icon "snow-night"

        MistDay ->
            icon "mist-day"

        MistNight ->
            icon "mist-night"

        NoIcon ->
            span [] [ text "No icon" ]

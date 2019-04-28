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
    i [ class className, class "wi", class "weather-icon" ] []


mapIcon : WeatherIcon -> Html msg
mapIcon weatherIcon =
    case weatherIcon of
        ClearSkyDay ->
            icon "wi-day-sunny"

        ClearSkyNight ->
            icon "wi-night-clear"

        FewCloudsDay ->
            icon "wi-day-cloudy"

        FewCloudsNight ->
            icon "wi-night-cloudy"

        ScatteredCloudsDay ->
            icon "wi-cloud"

        ScatteredCloudsNight ->
            icon "wi-cloud"

        BrokenCloudsDay ->
            icon "wi-cloudy"

        BrokenCloudsNight ->
            icon "wi-cloudy"

        ShowerRainDay ->
            icon "wi-day-rain"

        ShowerRainNight ->
            icon "wi-night-rain"

        RainDay ->
            icon "wi-day-rain"

        RainNight ->
            icon "wi-night-rain"

        ThunderstormDay ->
            icon "wi-thunderstorm"

        ThunderstormNight ->
            icon "wi-thunderstorm"

        SnowDay ->
            icon "wi-snow"

        SnowNight ->
            icon "wi-snow"

        MistDay ->
            icon "wi-fog"

        MistNight ->
            icon "wi-fog"

        NoIcon ->
            span [] [ text "No icon" ]

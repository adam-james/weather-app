module Page.City exposing (view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Page.City.CurrentWeather as CurrentWeatherPage
import Page.City.Forecast as ForecastPage
import Url
import Url.Parser as Parser exposing ((</>))



-- ROUTING


type Route
    = CurrentWeather Int
    | Forecast Int


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map CurrentWeather (Parser.s "city" </> Parser.int)
        , Parser.map CurrentWeather
            (Parser.s "city" </> Parser.int </> Parser.s "current-weather")
        , Parser.map Forecast
            (Parser.s "city" </> Parser.int </> Parser.s "forecast")
        ]


matchRoute : Url.Url -> Maybe Route
matchRoute url =
    Parser.parse routeParser url



-- VIEW


view : Url.Url -> { title : String, content : Html msg }
view url =
    case matchRoute url of
        Just (CurrentWeather cityId) ->
            baseView cityId CurrentWeatherPage.view

        Just (Forecast cityId) ->
            baseView cityId ForecastPage.view

        Nothing ->
            { title = "City - Not Found"
            , content = h1 [] [ text "Not Found" ]
            }


baseView : Int -> Html msg -> { title : String, content : Html msg }
baseView cityId subView =
    { title = "City - "
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

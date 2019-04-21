module Page.City exposing (view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Page.City.CurrentWeather as CurrentWeatherPage
import Page.City.Forecast as ForecastPage
import Url
import Url.Parser as Parser exposing ((</>))



-- ROUTING


type Route
    = CurrentWeather
    | Forecast


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map CurrentWeather (Parser.s "city")
        , Parser.map CurrentWeather (Parser.s "city" </> Parser.s "current-weather")
        , Parser.map Forecast (Parser.s "city" </> Parser.s "forecast")
        ]


matchRoute : Url.Url -> Maybe Route
matchRoute url =
    Parser.parse routeParser url



-- VIEW


view : Url.Url -> { title : String, content : Html msg }
view url =
    case matchRoute url of
        Just CurrentWeather ->
            baseView CurrentWeatherPage.view

        Just Forecast ->
            baseView ForecastPage.view

        Nothing ->
            baseView (h1 [] [ text "Not Found" ])


baseView : Html msg -> { title : String, content : Html msg }
baseView subView =
    { title = "City - "
    , content =
        div []
            [ ul []
                [ li [] [ a [ href "/city/current-weather" ] [ text "Current Weather" ] ]
                , li [] [ a [ href "/city/forecast" ] [ text "Forecast" ] ]
                ]
            , subView
            ]
    }

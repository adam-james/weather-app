module Forecast exposing
    ( Forecast
    , ForecastItem
    , Main
    , forecastDecoder
    )

import City exposing (City, cityDecoder)
import Json.Decode as Decode



-- MODEL


type alias ForecastItem =
    { datetime : Int
    , weathers : List Weather
    , main : Main
    }


type alias Forecast =
    { city : City
    , items : List ForecastItem
    }


type alias Weather =
    { id : Int
    , main : String
    , description : String
    , icon : String
    }


type alias Main =
    { temp : Float
    , pressure : Float
    , humidity : Float
    , tempMin : Float
    , tempMax : Float
    }



-- HTTP


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
    Decode.map3 ForecastItem
        (Decode.field "dt" Decode.int)
        (Decode.field "weather" weathersDecoder)
        (Decode.field "main" mainDecoder)


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


mainDecoder : Decode.Decoder Main
mainDecoder =
    Decode.map5 Main
        (Decode.field "temp" Decode.float)
        (Decode.field "pressure" Decode.float)
        (Decode.field "humidity" Decode.float)
        (Decode.field "temp_min" Decode.float)
        (Decode.field "temp_max" Decode.float)

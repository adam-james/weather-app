module Forecast exposing
    ( DayItems
    , Forecast
    , ForecastItem
    , Main
    , Weather
    , forecastDecoder
    , groupByDay
    , listMax
    , listMin
    , mean
    , mostCommon
    , sum
    , summarize
    )

import City exposing (City, cityDecoder)
import Dict
import Json.Decode as Decode
import Time



-- Model


type alias ForecastItem =
    { datetime : Int
    , weathers : List Weather
    , main : Main
    }


type alias Forecast =
    { city : City
    , items : List ForecastItem
    }


type alias DayItems =
    ( Int, List ForecastItem )


type alias DayForecast =
    { datetime : Int
    , summary : Summary
    }


type alias Summary =
    { tempMean : Float
    , tempMin : Float
    , tempMax : Float
    , icon : String
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



-- Utils
-- TODO secondsToPosix is duplicated, maybe use a serializer to turn seconds to posix or millis


secondsToPosix : Int -> Time.Posix
secondsToPosix seconds =
    seconds
        |> (*) 1000
        |> Time.millisToPosix


groupByDay : Time.Zone -> List ForecastItem -> List DayItems
groupByDay timezone items =
    let
        dict =
            List.foldl
                (\item carry ->
                    let
                        day =
                            item.datetime
                                |> secondsToPosix
                                |> Time.toDay timezone
                    in
                    Dict.update
                        day
                        (\existingDay ->
                            case existingDay of
                                Just existing ->
                                    Just (item :: existing)

                                Nothing ->
                                    Just [ item ]
                        )
                        carry
                )
                Dict.empty
                items
    in
    dict
        |> Dict.toList
        |> List.sortWith
            (\( day1, _ ) ( day2, _ ) ->
                -- handle month changeover
                if day1 < day2 then
                    if day2 - day1 > 5 then
                        GT

                    else
                        LT

                else
                    GT
            )


sum : List Float -> Float
sum numbers =
    List.foldl (+) 0 numbers


mean : List Float -> Float
mean numbers =
    sum numbers / toFloat (List.length numbers)


listMin : List Float -> Float
listMin numbers =
    -- assuming temp will never be higher than 1000
    List.foldl min 1000 numbers


listMax : List Float -> Float
listMax numbers =
    -- assuming temp will never be lower than -1000
    List.foldl max -1000 numbers


groupStrings : List String -> Dict.Dict String Int
groupStrings strings =
    strings
        |> List.foldr
            (\string carry ->
                Dict.update
                    string
                    (\existingCount ->
                        case existingCount of
                            Just existing ->
                                Just (existing + 1)

                            Nothing ->
                                Just 1
                    )
                    carry
            )
            Dict.empty


mostCommon : List String -> String
mostCommon strings =
    let
        counts =
            Dict.toList (groupStrings strings)

        ( string, count ) =
            List.foldl
                (\( thisString, thisCount ) ( carryString, carryCount ) ->
                    if thisCount > carryCount then
                        ( thisString, thisCount )

                    else
                        ( carryString, carryCount )
                )
                ( "", 0 )
                counts
    in
    string


summarize : List ForecastItem -> Summary
summarize items =
    let
        temps =
            List.map (\item -> item.main.temp) items

        icons =
            List.map
                (\item ->
                    case List.head item.weathers of
                        Just weather ->
                            weather.icon

                        Nothing ->
                            ""
                )
                items
    in
    { tempMean = mean temps
    , tempMin = listMin temps
    , tempMax = listMax temps
    , icon = mostCommon icons
    }



-- Decoders


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

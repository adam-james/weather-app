module ForecastTests exposing (all)

import Expect
import Forecast exposing (..)
import Fuzz
import Test exposing (..)



-- Fuzzers


weatherFuzzer : Fuzz.Fuzzer Weather
weatherFuzzer =
    Fuzz.map4 Weather
        (Fuzz.intRange 1 1000)
        Fuzz.string
        Fuzz.string
        Fuzz.string


mainFuzzer : Fuzz.Fuzzer Main
mainFuzzer =
    Fuzz.map5 Main
        -- temp within -1000 and 1000
        (Fuzz.floatRange -1000 1000)
        Fuzz.float
        Fuzz.float
        Fuzz.float
        Fuzz.float


itemFuzzer : Fuzz.Fuzzer ForecastItem
itemFuzzer =
    Fuzz.map3 ForecastItem
        Fuzz.int
        (Fuzz.list weatherFuzzer)
        mainFuzzer



-- Tests


all : Test
all =
    describe "Forecast"
        [ sumTest
        , meanTest
        , listMinTest
        , listMaxTest
        , summarizeTest
        , mostCommonTest
        ]


mostCommonTest : Test
mostCommonTest =
    describe "mostCommon"
        [ fuzz2 Fuzz.string Fuzz.string "finds the most common string in a list" <|
            \string1 string2 ->
                let
                    strings =
                        [ string1, string2, string1 ]
                in
                strings
                    |> mostCommon
                    |> Expect.equal string1
        ]


summarizeTest : Test
summarizeTest =
    describe "summarize"
        [ fuzz3 itemFuzzer itemFuzzer itemFuzzer "finds mean temp" <|
            \item1 item2 item3 ->
                let
                    items =
                        [ item1, item2, item3 ]

                    temps =
                        List.map (\item -> item.main.temp) items

                    tempMean =
                        (summarize items).tempMean
                in
                tempMean
                    |> Expect.within (Expect.Relative 0) (mean temps)
        , fuzz3 itemFuzzer itemFuzzer itemFuzzer "finds min temp" <|
            \item1 item2 item3 ->
                let
                    items =
                        [ item1, item2, item3 ]

                    temps =
                        List.map (\item -> item.main.temp) items

                    tempMin =
                        (summarize items).tempMin
                in
                tempMin
                    |> Expect.within (Expect.Relative 0) (listMin temps)
        , fuzz3 itemFuzzer itemFuzzer itemFuzzer "finds max temp" <|
            \item1 item2 item3 ->
                let
                    items =
                        [ item1, item2, item3 ]

                    temps =
                        List.map (\item -> item.main.temp) items

                    tempMax =
                        (summarize items).tempMax
                in
                tempMax
                    |> Expect.within (Expect.Relative 0) (listMax temps)
        , fuzz2 itemFuzzer itemFuzzer "finds most common icon" <|
            \item1 item2 ->
                let
                    items =
                        [ item1, item2, item1 ]

                    temps =
                        List.map (\item -> item.main.temp) items

                    item1Icon =
                        case List.head item1.weathers of
                            Just weather ->
                                weather.icon

                            Nothing ->
                                ""

                    icon =
                        (summarize items).icon
                in
                icon
                    |> Expect.equal item1Icon
        ]


sumTest : Test
sumTest =
    describe "sum"
        [ fuzz3 Fuzz.float Fuzz.float Fuzz.float "sums numbers" <|
            \one two three ->
                let
                    numbers =
                        [ one, two, three ]
                in
                numbers
                    |> sum
                    |> Expect.within (Expect.Relative 0) (one + two + three)
        ]


meanTest : Test
meanTest =
    describe "mean"
        [ fuzz3 Fuzz.float Fuzz.float Fuzz.float "averages numbers" <|
            \one two three ->
                let
                    numbers =
                        [ one, two, three ]
                in
                numbers
                    |> mean
                    |> Expect.within (Expect.Relative 0) ((one + two + three) / 3)
        ]


listMinTest : Test
listMinTest =
    describe "listMin"
        [ test "finds the min in the list" <|
            \_ ->
                let
                    numbers =
                        [ 1, 2, 3 ]
                in
                numbers
                    |> listMin
                    |> Expect.within (Expect.Relative 0) 1
        , test "won't select numbers over 1000" <|
            \_ ->
                let
                    numbers =
                        [ 1001 ]
                in
                numbers
                    |> listMin
                    |> Expect.within (Expect.Relative 0) 1000
        ]


listMaxTest : Test
listMaxTest =
    describe "listMax"
        [ test "finds the max in the list" <|
            \_ ->
                let
                    numbers =
                        [ 1, 2, 3 ]
                in
                numbers
                    |> listMax
                    |> Expect.within (Expect.Relative 0) 3
        , test "won't select numbers below 1001" <|
            \_ ->
                let
                    numbers =
                        [ -1001 ]
                in
                numbers
                    |> listMax
                    |> Expect.within (Expect.Relative 0) -1000
        ]

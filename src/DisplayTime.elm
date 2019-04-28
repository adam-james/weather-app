module DisplayTime exposing (displayDateTime)

import Time


displayDateTime : Time.Zone -> Int -> String
displayDateTime timezone seconds =
    let
        weekday =
            displayWeekday timezone seconds

        month =
            displayMonth timezone seconds

        day =
            displayDay timezone seconds

        year =
            displayYear timezone seconds

        time =
            displayTime timezone seconds
    in
    weekday
        ++ ", "
        ++ month
        ++ " "
        ++ day
        ++ ", "
        ++ year
        ++ " "
        ++ time


secondsToPosix : Int -> Time.Posix
secondsToPosix seconds =
    seconds
        |> (*) 1000
        |> Time.millisToPosix


displayTime : Time.Zone -> Int -> String
displayTime timezone seconds =
    let
        hour =
            displayHour timezone seconds

        minute =
            displayMinute timezone seconds
    in
    hour
        ++ ":"
        ++ minute


displayMinute : Time.Zone -> Int -> String
displayMinute timezone seconds =
    seconds
        |> secondsToPosix
        |> Time.toMinute timezone
        |> String.fromInt


displayHour : Time.Zone -> Int -> String
displayHour timezone seconds =
    seconds
        |> secondsToPosix
        |> Time.toHour timezone
        |> String.fromInt


displayYear : Time.Zone -> Int -> String
displayYear timezone seconds =
    seconds
        |> secondsToPosix
        |> Time.toYear timezone
        |> String.fromInt


displayWeekday : Time.Zone -> Int -> String
displayWeekday timezone seconds =
    seconds
        |> secondsToPosix
        -- TODO use browser timezone
        |> Time.toWeekday timezone
        |> weekdayToString


displayMonth : Time.Zone -> Int -> String
displayMonth timezone seconds =
    seconds
        |> secondsToPosix
        |> Time.toMonth timezone
        |> monthToString


displayDay : Time.Zone -> Int -> String
displayDay timezone seconds =
    seconds
        |> secondsToPosix
        |> Time.toDay timezone
        |> String.fromInt


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


weekdayToString : Time.Weekday -> String
weekdayToString weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"

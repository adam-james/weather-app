module TemperatureScale exposing
    ( TemperatureScale(..)
    , apiUnits
    , fromString
    , toString
    , unit
    )


type TemperatureScale
    = Fahrenheit
    | Celsius


toString : TemperatureScale -> String
toString tempScale =
    case tempScale of
        Fahrenheit ->
            "fahrenheit"

        Celsius ->
            "celsius"


fromString : String -> Maybe TemperatureScale
fromString str =
    case str of
        "fahrenheit" ->
            Just Fahrenheit

        "celsius" ->
            Just Celsius

        _ ->
            Nothing


unit : TemperatureScale -> String
unit tempScale =
    case tempScale of
        Fahrenheit ->
            "F°"

        Celsius ->
            "C°"


apiUnits : TemperatureScale -> String
apiUnits tempScale =
    case tempScale of
        Fahrenheit ->
            "imperial"

        Celsius ->
            "metric"

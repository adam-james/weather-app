module City exposing
    ( City
    , citiesDecoder
    , cityDecoder
    , fullName
    )

import Json.Decode as Decode


type alias City =
    { id : Int, name : String, country : String }


fullName : City -> String
fullName city =
    city.name ++ ", " ++ city.country


cityDecoder : Decode.Decoder City
cityDecoder =
    Decode.map3 City
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "country" Decode.string)


citiesDecoder : Decode.Decoder (List City)
citiesDecoder =
    Decode.list cityDecoder

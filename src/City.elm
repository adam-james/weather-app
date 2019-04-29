module City exposing (City, cityDecoder, citiesDecoder)

import Json.Decode as Decode


type alias City =
    { id : Int, name : String, country : String }


cityDecoder : Decode.Decoder City
cityDecoder =
    Decode.map3 City
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "country" Decode.string)


citiesDecoder : Decode.Decoder (List City)
citiesDecoder =
    Decode.list cityDecoder

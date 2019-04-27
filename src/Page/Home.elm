module Page.Home exposing
    ( City
    , Model
    , Msg(..)
    , init
    , update
    , view
    )

import Browser
import Combobox
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events as Evnts
import Http
import Json.Decode as Decode



---- MODEL ----


type alias City =
    { id : Int, name : String, country : String }


type alias Model =
    { textInput : String
    , options : Combobox.Model City
    , cities : List City
    }


init : ( Model, Cmd Msg )
init =
    ( { textInput = ""
      , options = Combobox.Collapsed
      , cities = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Input String
    | Up
    | Down
    | Enter
    | Escape
    | Blur
    | Focus
    | ClickCity City
    | GotCities (Result Http.Error (List City))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCities result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok cities ->
                    ( { model
                        | cities = cities
                        , options =
                            Combobox.Expanded
                                { activeOption = Nothing
                                , options = cities
                                }
                      }
                    , Cmd.none
                    )

        ClickCity city ->
            ( { model
                | options = Combobox.Selected city
                , textInput = city.name
              }
            , Cmd.none
            )

        Focus ->
            case String.length model.textInput of
                0 ->
                    ( model, Cmd.none )

                _ ->
                    case List.length model.cities of
                        0 ->
                            ( model, Cmd.none )

                        _ ->
                            ( { model
                                | options =
                                    Combobox.Expanded
                                        { activeOption = Nothing
                                        , options = model.cities
                                        }
                              }
                            , Cmd.none
                            )

        Blur ->
            let
                cities =
                    case model.options of
                        Combobox.Selected _ ->
                            model.options

                        _ ->
                            Combobox.Collapsed
            in
            ( { model
                | options = cities
              }
            , Cmd.none
            )

        Escape ->
            ( { model
                | textInput = ""
                , options = Combobox.Collapsed
              }
            , Cmd.none
            )

        Enter ->
            let
                cities =
                    case model.options of
                        Combobox.Collapsed ->
                            Combobox.Collapsed

                        Combobox.Expanded { activeOption, options } ->
                            case activeOption of
                                Nothing ->
                                    Combobox.Expanded
                                        { activeOption = activeOption
                                        , options = options
                                        }

                                Just city ->
                                    Combobox.Selected city

                        Combobox.Selected selected ->
                            Combobox.Selected selected

                textInput =
                    case cities of
                        Combobox.Selected selected ->
                            selected.name

                        _ ->
                            model.textInput
            in
            ( { model
                | options = cities
                , textInput = textInput
              }
            , Cmd.none
            )

        Up ->
            let
                cities =
                    Combobox.activatePrevious model.options
            in
            ( { model | options = cities }, Cmd.none )

        Down ->
            let
                cities =
                    Combobox.activateNext model.options
            in
            ( { model | options = cities }, Cmd.none )

        Input textInput ->
            case String.length textInput of
                0 ->
                    ( { model | textInput = textInput }, Cmd.none )

                _ ->
                    ( { model | textInput = textInput }, getCities textInput )

        NoOp ->
            ( model, Cmd.none )



-- DECODERS
-- TODO Move this to combobox
-- toDirection can take 4 more args of type `msg`


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "keyCode" Decode.int)


toDirection : Int -> Msg
toDirection keyCode =
    case keyCode of
        38 ->
            Up

        40 ->
            Down

        13 ->
            Enter

        27 ->
            Escape

        _ ->
            NoOp



---- VIEW ----


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content = content model
    }


content : Model -> Html Msg
content model =
    Combobox.container model.options
        listitemId
        [ Combobox.comboboxLabel "Find a city"
        , Combobox.textbox model.options
            [ Evnts.onInput Input
            , Evnts.on "keydown" <| keyDecoder
            , Attrs.value (inputValue model)
            , Evnts.onBlur Blur
            , Evnts.onFocus Focus
            ]
        , Combobox.listbox model.options (viewOptions model)
        ]


viewOptions : Model -> List (Html Msg)
viewOptions model =
    case model.options of
        Combobox.Expanded { activeOption, options } ->
            options
                |> List.map
                    (Combobox.option
                        model.options
                        listitemId
                        .name
                        (\city -> [ Evnts.onMouseDown (ClickCity city) ])
                    )

        Combobox.Collapsed ->
            []

        Combobox.Selected _ ->
            []


inputValue : Model -> String
inputValue model =
    case model.options of
        Combobox.Selected selected ->
            selected.name

        _ ->
            model.textInput


listitemId : City -> String
listitemId city =
    "city-" ++ String.fromInt city.id



-- HTTP


getCities : String -> Cmd Msg
getCities query =
    Http.get
        { url = "http://localhost:5000/cities?name=" ++ query
        , expect = Http.expectJson GotCities citiesDecoder
        }


cityDecoder : Decode.Decoder City
cityDecoder =
    Decode.map3 City
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "country" Decode.string)


citiesDecoder : Decode.Decoder (List City)
citiesDecoder =
    Decode.list cityDecoder

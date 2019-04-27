module Page.Home exposing (City, Model, Msg(..), init, update, view)

import Browser
import Combobox
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events as Evnts
import Json.Decode as Decode



-- UTILS


filterCities : String -> List City -> List City
filterCities query cities =
    List.filter
        (\city ->
            String.contains (String.toLower query) (String.toLower city.name)
        )
        cities



---- MODEL ----


type alias City =
    { id : Int, name : String }


initialCities : List City
initialCities =
    [ { id = 1, name = "San Francisco" }
    , { id = 2, name = "San Diego" }
    , { id = 3, name = "Sacramento" }
    , { id = 4, name = "San Jose" }
    , { id = 5, name = "Santa Barbara" }
    ]


type alias Model =
    { textInput : String
    , cities : Combobox.Model City
    }


init : ( Model, Cmd Msg )
init =
    ( { textInput = ""
      , cities = Combobox.Collapsed
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickCity city ->
            ( { model
                | cities = Combobox.Selected city
                , textInput = city.name
              }
            , Cmd.none
            )

        Focus ->
            case String.length model.textInput of
                0 ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | cities =
                            Combobox.Expanded
                                { activeOption = Nothing
                                , options = filterCities model.textInput initialCities
                                }
                      }
                    , Cmd.none
                    )

        Blur ->
            let
                cities =
                    case model.cities of
                        Combobox.Selected _ ->
                            model.cities

                        _ ->
                            Combobox.Collapsed
            in
            ( { model
                | cities = cities
              }
            , Cmd.none
            )

        Escape ->
            ( { model
                | textInput = ""
                , cities = Combobox.Collapsed
              }
            , Cmd.none
            )

        Enter ->
            let
                cities =
                    case model.cities of
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
                | cities = cities
                , textInput = textInput
              }
            , Cmd.none
            )

        Up ->
            let
                cities =
                    Combobox.activatePrevious model.cities
            in
            ( { model | cities = cities }, Cmd.none )

        Down ->
            let
                cities =
                    Combobox.activateNext model.cities
            in
            ( { model | cities = cities }, Cmd.none )

        Input textInput ->
            let
                cities =
                    case String.length textInput of
                        0 ->
                            Combobox.Collapsed

                        _ ->
                            Combobox.Expanded
                                { activeOption = Nothing
                                , options = filterCities textInput initialCities
                                }
            in
            ( { model
                | textInput = textInput
                , cities = cities
              }
            , Cmd.none
            )

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
    Combobox.container model.cities
        listitemId
        [ Combobox.textbox model.cities
            [ Evnts.onInput Input
            , Evnts.on "keydown" <| keyDecoder
            , Attrs.value (inputValue model)
            , Evnts.onBlur Blur
            , Evnts.onFocus Focus
            ]
        , Combobox.listbox model.cities (viewOptions model)
        ]


viewOptions : Model -> List (Html Msg)
viewOptions model =
    case model.cities of
        Combobox.Expanded { activeOption, options } ->
            options
                |> filterCities model.textInput
                |> List.map
                    (Combobox.option
                        model.cities
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
    case model.cities of
        Combobox.Selected selected ->
            selected.name

        _ ->
            model.textInput


listitemId : City -> String
listitemId city =
    "city-" ++ String.fromInt city.id

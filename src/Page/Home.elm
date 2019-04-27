module Page.Home exposing
    ( City
    , Model
    , Msg(..)
    , init
    , update
    , view
    )

import Browser
import Browser.Navigation as Nav
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
    , key : Nav.Key
    }


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { textInput = ""
      , options = Combobox.Collapsed
      , cities = []
      , key = key
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Input String
    | Up
    | Down
    | Escape
    | Blur
    | ClickCity City
    | GotCities (Result Http.Error (List City))
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            case model.options of
                Combobox.Selected selectedCity ->
                    ( model
                    , Nav.pushUrl model.key
                        ("/city/" ++ String.fromInt selectedCity.id)
                    )

                Combobox.Expanded { activeOption } ->
                    case activeOption of
                        Just active ->
                            ( { model
                                | options = Combobox.Selected active
                                , textInput = active.name
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Combobox.Collapsed ->
                    ( model, Cmd.none )

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
                    ( { model
                        | textInput = textInput
                        , cities = []
                        , options = Combobox.Collapsed
                      }
                    , Cmd.none
                    )

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
    form [ Evnts.onSubmit Submit ]
        [ Combobox.container
            model.options
            listitemId
            [ Combobox.comboboxLabel "Find a city"
            , Combobox.textbox model.options
                [ Evnts.onInput Input
                , Evnts.on "keydown" <| keyDecoder
                , Attrs.value (inputValue model)
                , Evnts.onBlur Blur
                ]
            , Combobox.listbox model.options (viewOptions model)
            ]
        , button [] [ text "Go" ]
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

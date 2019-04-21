module Page.Home exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (..)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    { count : Int }


init : Model
init =
    { count = 0 }



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { count = model.count + 1 }

        Decrement ->
            { count = model.count - 1 }


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div []
            [ h1 [] [ text ("The count is: " ++ String.fromInt model.count) ]
            , button [ onClick Decrement ] [ text "Decrement" ]
            , button [ onClick Increment ] [ text "Increment" ]
            ]
    }

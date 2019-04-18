module Page.Home exposing (view)

import Html exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "Home"
    , content = h1 [] [ text "Home Page" ]
    }

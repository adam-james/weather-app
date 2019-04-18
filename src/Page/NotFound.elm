module Page.NotFound exposing (view)

import Browser
import Html exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "Not Found"
    , content = h1 [] [ text "Not Found " ]
    }

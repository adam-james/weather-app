module Page.About exposing (view)

import Html exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "About"
    , content =
        main_ []
            [ h2 [] [ text "About" ]
            , p [] [ text "A weather app." ]
            ]
    }

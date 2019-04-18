module Page exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href)


view : { title : String, content : Html msg } -> Browser.Document msg
view page =
    { title = "Pages - " ++ page.title
    , body =
        [ header [ class "page-header" ]
            [ ul [ class "page-header__nav" ]
                [ li [ class "page-header__nav-item" ]
                    [ a [ href "/", class "page-header__nav-link" ] [ text "Home" ] ]
                , li [ class "page-header__nav-item" ]
                    [ a [ href "/about", class "page-header__nav-link" ] [ text "About" ] ]
                ]
            ]
        , page.content
        ]
    }

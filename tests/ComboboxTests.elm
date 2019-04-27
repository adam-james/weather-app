module ComboboxTests exposing (updateTests, viewTests)

import Combobox exposing (..)
import Expect
import Fuzz
import Html
import Html.Attributes as Attrs
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector



-- TODO spec this
--  If the combobox has a visible label, the element with role combobox has aria-labelledby set to a value that refers to the labeling element. Otherwise, the combobox element has a label provided by aria-label.
-- Data


type alias City =
    { id : Int, name : String }


cityFuzzer : Fuzz.Fuzzer City
cityFuzzer =
    Fuzz.map2 City
        (Fuzz.intRange 1 1000)
        Fuzz.string


berlin =
    { id = 1, name = "Berlin" }


madrid =
    { id = 2, name = "Madrid" }


testCities =
    [ berlin, madrid ]



-- Update


updateTests : Test
updateTests =
    describe "updates"
        [ activateNext
        , activatePrevious
        ]


activateNext : Test
activateNext =
    describe "activate next"
        [ test "does nothing when collapsed" <|
            \_ ->
                Combobox.Collapsed
                    |> Combobox.activateNext
                    |> Expect.equal Combobox.Collapsed
        , fuzz3 cityFuzzer cityFuzzer cityFuzzer "activates head when no active option" <|
            \city1 city2 city3 ->
                let
                    cities =
                        [ city1, city2, city3 ]
                in
                Combobox.Expanded { activeOption = Nothing, options = cities }
                    |> Combobox.activateNext
                    |> Expect.equal
                        (Combobox.Expanded
                            { activeOption = Just city1
                            , options = cities
                            }
                        )
        , fuzz3 cityFuzzer cityFuzzer cityFuzzer "activates next when active option" <|
            \city1 city2 city3 ->
                let
                    cities =
                        [ city1, city2, city3 ]
                in
                Combobox.Expanded { activeOption = Just city1, options = cities }
                    |> Combobox.activateNext
                    |> Expect.equal
                        (Combobox.Expanded
                            { activeOption = Just city2
                            , options = cities
                            }
                        )
        , fuzz3 cityFuzzer cityFuzzer cityFuzzer "activates first when last item is active" <|
            \city1 city2 city3 ->
                let
                    cities =
                        [ city1, city2, city3 ]
                in
                Combobox.Expanded { activeOption = Just city3, options = cities }
                    |> Combobox.activateNext
                    |> Expect.equal
                        (Combobox.Expanded
                            { activeOption = Just city1
                            , options = cities
                            }
                        )
        ]


activatePrevious : Test
activatePrevious =
    describe "activatePrevious"
        [ test "does nothing when collapsed" <|
            \_ ->
                Combobox.Collapsed
                    |> Combobox.activatePrevious
                    |> Expect.equal Combobox.Collapsed
        , fuzz3 cityFuzzer cityFuzzer cityFuzzer "activates last option when none active" <|
            \city1 city2 city3 ->
                let
                    cities =
                        [ city1, city2, city3 ]
                in
                Combobox.Expanded { activeOption = Nothing, options = cities }
                    |> Combobox.activatePrevious
                    |> Expect.equal
                        (Combobox.Expanded
                            { activeOption = Just city3, options = cities }
                        )
        , fuzz3 cityFuzzer cityFuzzer cityFuzzer "activates previous option" <|
            \city1 city2 city3 ->
                let
                    cities =
                        [ city1, city2, city3 ]
                in
                Combobox.Expanded { activeOption = Just city2, options = cities }
                    |> Combobox.activatePrevious
                    |> Expect.equal
                        (Combobox.Expanded
                            { activeOption = Just city1, options = cities }
                        )
        , fuzz3 cityFuzzer cityFuzzer cityFuzzer "activates last when first active" <|
            \city1 city2 city3 ->
                let
                    cities =
                        [ city1, city2, city3 ]
                in
                Combobox.Expanded { activeOption = Just city1, options = cities }
                    |> Combobox.activatePrevious
                    |> Expect.equal
                        (Combobox.Expanded
                            { activeOption = Just city3, options = cities }
                        )
        ]



-- View


testWrapper : Html.Html msg -> Html.Html msg
testWrapper child =
    Html.div [] [ child ]


viewTests : Test
viewTests =
    describe "view"
        [ containerTests
        , listboxTests
        , optionTests
        , textboxTests
        ]


textboxTests : Test
textboxTests =
    describe "textbox"
        [ test "sets 'aria-autocomplete' to 'list'" <|
            \_ ->
                testWrapper (textbox Combobox.Collapsed [])
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "input"
                        , Selector.attribute
                            (Attrs.attribute "aria-autocomplete" "list")
                        ]
                    |> Query.has []
        , test "controls listbox when expanded" <|
            \_ ->
                testWrapper
                    (textbox
                        (Combobox.Expanded
                            (Combobox.OptionList Nothing testCities)
                        )
                        []
                    )
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "input"
                        , Selector.attribute <| Attrs.attribute "aria-controls" "combobox-listbox"
                        ]
                    |> Query.has []
        , test "sets autocomplete to off" <|
            \_ ->
                testWrapper (textbox Combobox.Collapsed [])
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "input"
                        , Selector.attribute
                            (Attrs.attribute "autocomplete" "off")
                        ]
                    |> Query.has []
        ]


listboxTests : Test
listboxTests =
    describe "listbox"
        [ test "has role 'listbox'" <|
            \_ ->
                testWrapper (listbox Combobox.Collapsed [])
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "ul"
                        , Selector.attribute
                            (Attrs.attribute "role" "listbox")
                        ]
                    |> Query.has []
        ]


optionTests : Test
optionTests =
    describe "option"
        [ test "sets id" <|
            \_ ->
                testWrapper
                    (option
                        (Combobox.Expanded
                            (Combobox.OptionList Nothing testCities)
                        )
                        (\city -> "city-" ++ String.fromInt city.id)
                        .name
                        (\city -> [])
                        berlin
                    )
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "li"
                        ]
                    |> Query.has [ Selector.id "city-1" ]
        , test "renders text" <|
            \_ ->
                testWrapper
                    (option
                        (Combobox.Expanded
                            (Combobox.OptionList Nothing testCities)
                        )
                        (\city -> "city-" ++ String.fromInt city.id)
                        .name
                        (\city -> [])
                        berlin
                    )
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "li"
                        ]
                    |> Query.has [ Selector.text "Berlin" ]
        , test "has 'role' of 'option'" <|
            \_ ->
                testWrapper
                    (option
                        (Combobox.Expanded
                            (Combobox.OptionList Nothing testCities)
                        )
                        (\city -> "city-" ++ String.fromInt city.id)
                        .name
                        (\city -> [])
                        berlin
                    )
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "li"
                        ]
                    |> Query.has [ Selector.attribute (Attrs.attribute "role" "option") ]
        , test "renders active class when active" <|
            \_ ->
                testWrapper
                    (option
                        (Combobox.Expanded
                            (Combobox.OptionList (Just berlin) testCities)
                        )
                        (\city -> "city-" ++ String.fromInt city.id)
                        .name
                        (\city -> [])
                        berlin
                    )
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "li"
                        ]
                    |> Query.has [ Selector.class "options__option--active" ]
        , test "sets 'aria-selected' to 'true' when active" <|
            \_ ->
                testWrapper
                    (option
                        (Combobox.Expanded
                            (Combobox.OptionList (Just berlin) testCities)
                        )
                        (\city -> "city-" ++ String.fromInt city.id)
                        .name
                        (\city -> [])
                        berlin
                    )
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "li"
                        ]
                    |> Query.has [ Selector.attribute (Attrs.attribute "aria-selected" "true") ]
        ]


containerTests : Test
containerTests =
    describe "container"
        [ test "has role 'combobox'" <|
            \_ ->
                testWrapper
                    (container
                        Combobox.Collapsed
                        (\city -> "city-" ++ String.fromInt city.id)
                        []
                    )
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "div" ]
                    |> Query.has [ Selector.attribute (Attrs.attribute "role" "combobox") ]
        , test "sets 'aria-expanded' to 'false'" <|
            \_ ->
                testWrapper
                    (container
                        Combobox.Collapsed
                        (\city -> "city-" ++ String.fromInt city.id)
                        []
                    )
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "div" ]
                    |> Query.has [ Selector.attribute (Attrs.attribute "aria-expanded" "false") ]
        , test "sets 'aria-activedescendant' when expanded" <|
            \_ ->
                testWrapper
                    (container
                        (Combobox.Expanded
                            (Combobox.OptionList (Just berlin) testCities)
                        )
                        (\city -> "city-" ++ String.fromInt city.id)
                        []
                    )
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "div" ]
                    |> Query.has
                        [ Selector.attribute
                            (Attrs.attribute "aria-activedescendant" "city-1")
                        ]
        , test "sets 'aria-expanded' to 'true' when expanded" <|
            \_ ->
                testWrapper
                    (container
                        (Combobox.Expanded
                            (Combobox.OptionList Nothing testCities)
                        )
                        (\city -> "city-" ++ String.fromInt city.id)
                        []
                    )
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "div" ]
                    |> Query.has
                        [ Selector.attribute
                            (Attrs.attribute "aria-expanded" "true")
                        ]
        ]

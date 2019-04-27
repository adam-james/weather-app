module Combobox exposing
    ( Model
    , OptionList
    , Selectable(..)
    , activateNext
    , activatePrevious
    , comboboxLabel
    , container
    , listbox
    , option
    , textbox
    )

import Html exposing (..)
import Html.Attributes as Attrs



-- Model


type alias OptionList a =
    { activeOption : Maybe a
    , options : List a
    }


type Selectable a
    = Expanded (OptionList a)
    | Selected a
    | Collapsed


type alias Model a =
    Selectable a



-- Update


activateNext : Model a -> Model a
activateNext model =
    activate model List.head findNext


activatePrevious : Model a -> Model a
activatePrevious model =
    let
        whenActive options =
            List.head (List.reverse options)
    in
    activate model whenActive findPrevious


activate : Model a -> (List a -> Maybe a) -> (a -> List a -> Maybe a) -> Model a
activate model whenActive whenNoActive =
    case model of
        Collapsed ->
            Collapsed

        Expanded { activeOption, options } ->
            case activeOption of
                Nothing ->
                    Expanded
                        { activeOption = whenActive options
                        , options = options
                        }

                Just active ->
                    Expanded
                        { activeOption = whenNoActive active options
                        , options = options
                        }

        Selected _ ->
            model


findNext : a -> List a -> Maybe a
findNext active all =
    doFindNext active all all


findPrevious : a -> List a -> Maybe a
findPrevious active all =
    let
        reversed =
            List.reverse all
    in
    findNext active reversed


doFindNext : a -> List a -> List a -> Maybe a
doFindNext active all remaining =
    case remaining of
        [] ->
            Nothing

        [ head ] ->
            if head == active then
                List.head all

            else
                Nothing

        head :: tail ->
            if head == active then
                List.head tail

            else
                doFindNext active all tail



-- View


textbox : Model a -> List (Attribute msg) -> Html msg
textbox model attrs =
    let
        baseAttrs =
            [ Attrs.type_ "text"
            , Attrs.autocomplete False
            , ariaAutocomplete "list"
            ]

        ownAttrs =
            case model of
                Expanded _ ->
                    ariaControls listboxId :: baseAttrs

                Collapsed ->
                    baseAttrs

                Selected _ ->
                    baseAttrs

        finalAttrs =
            ownAttrs ++ attrs
    in
    input finalAttrs []


listboxId : String
listboxId =
    "combobox-listbox"


listbox : Model a -> List (Html msg) -> Html msg
listbox model children =
    ul [ Attrs.id listboxId, role "listbox" ] children


labelId : String
labelId =
    "combobox-label"


comboboxLabel : String -> Html msg
comboboxLabel txt =
    label [ Attrs.id labelId, Attrs.class "combobox__label" ] [ text txt ]


option : Model a -> (a -> String) -> (a -> String) -> (a -> List (Attribute msg)) -> a -> Html msg
option model getId getText getAttrs item =
    let
        -- TODO allow classnames to be specified
        baseClass =
            "options__option"

        ( class, selected ) =
            case model of
                Expanded options ->
                    case options.activeOption of
                        Just active ->
                            if item == active then
                                ( baseClass ++ " options__option--active", "true" )

                            else
                                ( baseClass, "false" )

                        Nothing ->
                            ( baseClass, "false" )

                Collapsed ->
                    ( "", "false" )

                Selected _ ->
                    ( "", "false" )
    in
    li
        ([ Attrs.id (getId item)
         , role "option"
         , Attrs.class class
         , ariaSelected selected
         ]
            -- TODO is there a better way to do this?
            -- What is the cost of concatenating lists in Elm?
            -- This is a bit lazy. Maybe just pass events that are needed.
            ++ getAttrs item
        )
        [ text (getText item) ]


container : Model a -> (a -> String) -> List (Html msg) -> Html msg
container model getId children =
    let
        baseAttrs =
            [ role "combobox", ariaLabelledby labelId ]

        attrs =
            case model of
                Expanded options ->
                    case options.activeOption of
                        Just active ->
                            ariaActiveDescendant (getId active)
                                :: ariaExpanded "true"
                                :: baseAttrs

                        Nothing ->
                            ariaControls listboxId :: (ariaExpanded "true" :: baseAttrs)

                Collapsed ->
                    ariaExpanded "false" :: baseAttrs

                Selected _ ->
                    ariaExpanded "false" :: baseAttrs
    in
    div attrs children



-- Aria


role : String -> Html.Attribute msg
role =
    Attrs.attribute "role"


ariaExpanded : String -> Html.Attribute msg
ariaExpanded =
    Attrs.attribute "aria-expanded"


ariaControls : String -> Html.Attribute msg
ariaControls =
    Attrs.attribute "aria-controls"


ariaActiveDescendant : String -> Html.Attribute msg
ariaActiveDescendant =
    Attrs.attribute "aria-activedescendant"


ariaSelected : String -> Html.Attribute msg
ariaSelected =
    Attrs.attribute "aria-selected"


ariaAutocomplete : String -> Html.Attribute msg
ariaAutocomplete =
    Attrs.attribute "aria-autocomplete"


ariaLabelledby : String -> Html.Attribute msg
ariaLabelledby =
    Attrs.attribute "aria-labelledby"

module Main exposing (main)

import Autocomplete
import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Task


type alias Model =
    { focus : Maybe FormField

    -- FIELDS VALUES
    , field1 : String
    , field2 : String

    -- AUTOCOMPLETE 1
    , autocomplete1 : AutocompleteModel

    -- AUTOCOMPLETE 2
    , autocomplete2 : AutocompleteModel
    }


type alias AutocompleteModel =
    { fieldValue : String
    , fieldId : String
    , menuItems : List MenuItem
    , state : Autocomplete.State
    , howManyToShow : Int
    , selectedMenuItem : Maybe MenuItem
    , showMenu : Bool
    , focus : Bool
    , preventingBlur : Bool
    , preventingFocus : Bool
    }


init : ( Model, Cmd msg )
init =
    ( { -- FOCUS
        focus = Nothing

      -- FIELDS VALUES
      , field1 = ""
      , field2 = ""

      -- AUTOCOMPLETE 1
      , autocomplete1 =
            { fieldValue = ""
            , fieldId = "id99"
            , menuItems = menuItems1
            , state = Autocomplete.empty
            , howManyToShow = 50
            , selectedMenuItem = Nothing
            , showMenu = False
            , focus = False
            , preventingBlur = False
            , preventingFocus = False
            }

      -- AUTOCOMPLETE 2
      , autocomplete2 =
            { fieldValue = ""
            , fieldId = "id999"
            , menuItems = menuItems2
            , state = Autocomplete.empty
            , howManyToShow = 50
            , selectedMenuItem = Nothing
            , showMenu = False
            , focus = False
            , preventingBlur = False
            , preventingFocus = False
            }
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | OnFocus FormField
    | OnBlur FormField
    | SetField FormField String
      -- AUTOCOMPLETE
    | MsgAutocom String MsgAutocom


type FormField
    = Field1
    | Field2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        SetField field value ->
            ( model
                |> setField field value
            , Cmd.none
            )

        OnFocus formField ->
            ( { model | focus = Just formField }, Cmd.none )

        OnBlur formField ->
            ( { model | focus = Nothing }, Cmd.none )

        -- AUTOCOMPLETE
        MsgAutocom msgId msg ->
            if msgId == model.autocomplete1.fieldId then
                let
                    ( newModel, newMsg ) =
                        updateAutocom msg model.autocomplete1
                in
                ( { model | autocomplete1 = newModel }, newMsg )
            else if msgId == model.autocomplete2.fieldId then
                let
                    ( newModel, newMsg ) =
                        updateAutocom msg model.autocomplete2
                in
                ( { model | autocomplete2 = newModel }, newMsg )
            else
                ( model, Cmd.none )



-- HELPERS


setField : FormField -> String -> Model -> Model
setField field value model =
    case field of
        Field1 ->
            { model | field1 = value }

        Field2 ->
            { model | field2 = value }



-- VIEWS


view : Model -> Html Msg
view model =
    div [ class "form-container" ]
        [ div
            []
            [ label []
                [ h2 [] [ text "#1 Normal" ]
                , ul []
                    [ li [] [ text <| "Focus: " ++ toString model.focus ]
                    , li [] [ text <| "Content: " ++ model.field1 ]
                    ]
                , input
                    [ type_ "text"
                    , onInput <| SetField Field1
                    , onFocus <| OnFocus Field1
                    , onBlur <| OnBlur Field1
                    , value <| model.field1
                    ]
                    []
                ]
            , label []
                [ h2 [] [ text "#2 Using <datalist>" ]
                , ul []
                    [ li [] [ text <| "Focus: " ++ toString model.focus ]
                    , li [] [ text <| "Content: " ++ model.field2 ]
                    ]
                , input
                    [ type_ "text"
                    , onInput <| SetField Field2
                    , onFocus <| OnFocus Field2
                    , onBlur <| OnBlur Field2
                    , value <| model.field2
                    , list "programmingLanguage"
                    ]
                    []
                , datalist [ id "programmingLanguage" ]
                    (List.map
                        (\item -> option [ value item ] [])
                        menuItems1
                    )
                ]
            , viewAutocomplete model.autocomplete1
            , viewAutocomplete model.autocomplete2
            ]
        ]


viewAutocomplete : AutocompleteModel -> Html Msg
viewAutocomplete model =
    label []
        [ h2 [] [ text "#3 Using elm-autocomplete" ]
        , ul []
            [ li [] [ text <| "Focus: " ++ toString model.focus ]
            , li [] [ text <| "Value: " ++ model.fieldValue ]
            , li [] [ text <| "State:" ++ toString model.state ]
            , li [] [ text <| "HowMany:" ++ toString model.howManyToShow ]
            , li [] [ text <| "Selected:" ++ toString model.selectedMenuItem ]
            , li [] [ text <| "ShowMenu:" ++ toString model.showMenu ]
            ]
        , viewAutocom model
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map (MsgAutocom model.autocomplete1.fieldId << SetAutoState) Autocomplete.subscription
        , Sub.map (MsgAutocom model.autocomplete2.fieldId << SetAutoState) Autocomplete.subscription
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- AUTOCOMPLETE


type MsgAutocom
    = SetQuery String
    | SetAutoState Autocomplete.Msg
    | Wrap Bool
    | SelectMenuItemKeyboard String
    | SelectMenuItemMouse String
    | PreviewMenuItem String
    | HandleEscape
    | OnFocusAutocom
    | OnBlurAutocom
    | PreventFocusAndBlur
    | Reset
    | NoOpAutocom


updateAutocom : MsgAutocom -> AutocompleteModel -> ( AutocompleteModel, Cmd Msg )
updateAutocom msg model =
    case msg of
        SetQuery newQuery ->
            let
                showMenu =
                    not << List.isEmpty <| acceptableItems newQuery model.menuItems
            in
            { model
                | fieldValue = newQuery
                , showMenu = showMenu
                , selectedMenuItem = Nothing
            }
                ! []

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update (updateConfig model.fieldId) autoMsg model.howManyToShow model.state (acceptableItems model.fieldValue model.menuItems)

                newModel =
                    { model | state = newState }
            in
            case maybeMsg of
                Nothing ->
                    newModel ! []

                Just updateMsg ->
                    -- updateMsg need to be conerted from Msg -> MsgAutocom
                    case updateMsg of
                        MsgAutocom msgId autocomMsg ->
                            updateAutocom autocomMsg newModel

                        _ ->
                            newModel ! []

        Wrap toTop ->
            case model.selectedMenuItem of
                Just menuItem ->
                    updateAutocom Reset model

                Nothing ->
                    if toTop then
                        { model
                            | state = Autocomplete.resetToLastItem (updateConfig model.fieldId) (acceptableItems model.fieldValue model.menuItems) model.howManyToShow model.state
                            , selectedMenuItem = List.head <| List.reverse <| List.take model.howManyToShow <| acceptableItems model.fieldValue model.menuItems
                        }
                            ! []
                    else
                        { model
                            | state = Autocomplete.resetToFirstItem (updateConfig model.fieldId) (acceptableItems model.fieldValue model.menuItems) model.howManyToShow model.state
                            , selectedMenuItem = List.head <| List.take model.howManyToShow <| acceptableItems model.fieldValue model.menuItems
                        }
                            ! []

        SelectMenuItemKeyboard id ->
            let
                newModel =
                    setQuery model id
                        |> resetMenu
            in
            newModel ! []

        SelectMenuItemMouse id ->
            let
                newModel =
                    setQuery model id
                        |> resetMenu

                msgId =
                    99
            in
            -- Giving the focus again (Dom.focus) because it was lost upon
            -- the click on the menu
            ( newModel, Task.attempt (\_ -> MsgAutocom model.fieldId NoOpAutocom) (Dom.focus model.fieldId) )

        PreviewMenuItem id ->
            { model | selectedMenuItem = Just <| getMenuItemAtId model.menuItems id } ! []

        HandleEscape ->
            let
                validOptions =
                    not <| List.isEmpty (acceptableItems model.fieldValue model.menuItems)

                handleEscape =
                    if validOptions then
                        model
                            |> removeSelection
                            |> resetMenu
                    else
                        model
                            |> resetInput

                escapedModel =
                    case model.selectedMenuItem of
                        Just menuItem ->
                            if model.fieldValue == menuItem then
                                model
                                    |> resetInput
                            else
                                handleEscape

                        Nothing ->
                            handleEscape
            in
            escapedModel ! []

        Reset ->
            { model | state = Autocomplete.reset (updateConfig model.fieldId) model.state, selectedMenuItem = Nothing }
                ! []

        OnFocusAutocom ->
            -- lucamug -
            if model.preventingFocus then
                { model | preventingFocus = False } ! []
            else
                { model | focus = True, showMenu = True } ! []

        OnBlurAutocom ->
            -- lucamug - Closing the drop down on Blur. Not sure if resetMenu is the right
            -- thing to do here. Maybe not, I just want to close the menu.
            -- ({ model | focus = False } |> resetMenu) ! []
            if model.preventingBlur then
                { model | preventingBlur = False } ! []
            else
                { model | focus = False, showMenu = False } ! []

        PreventFocusAndBlur ->
            { model | preventingFocus = True, preventingBlur = True } ! []

        NoOpAutocom ->
            model ! []


resetInput : AutocompleteModel -> AutocompleteModel
resetInput model =
    { model | fieldValue = "" }
        |> removeSelection
        |> resetMenu


resetMenu : AutocompleteModel -> AutocompleteModel
resetMenu model =
    { model
        | state = Autocomplete.empty
        , showMenu = False
    }


removeSelection : AutocompleteModel -> AutocompleteModel
removeSelection model =
    { model | selectedMenuItem = Nothing }


getMenuItemAtId : List MenuItem -> String -> MenuItem
getMenuItemAtId menuItems id =
    List.filter (\menuItem -> menuItem == id) menuItems
        |> List.head
        |> Maybe.withDefault ""


setQuery : AutocompleteModel -> String -> AutocompleteModel
setQuery model id =
    { model
        | fieldValue = getMenuItemAtId model.menuItems id
        , selectedMenuItem = Just <| getMenuItemAtId model.menuItems id
    }


viewAutocom : AutocompleteModel -> Html Msg
viewAutocom model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            Decode.map
                (\code ->
                    if code == 38 || code == 40 then
                        Ok (MsgAutocom model.fieldId NoOpAutocom)
                    else if code == 27 then
                        Ok (MsgAutocom model.fieldId HandleEscape)
                    else
                        Err "not handling that key"
                )
                keyCode
                |> Decode.andThen
                    fromResult

        fromResult : Result String a -> Decode.Decoder a
        fromResult result =
            case result of
                Ok val ->
                    Decode.succeed val

                Err reason ->
                    Decode.fail reason

        menu =
            if model.showMenu then
                [ viewMenu model ]
            else
                []

        fieldValue =
            case model.selectedMenuItem of
                Just menuItem ->
                    menuItem

                Nothing ->
                    model.fieldValue

        activeDescendant attributes =
            case model.selectedMenuItem of
                Just menuItem ->
                    attribute "aria-activedescendant"
                        menuItem
                        :: attributes

                Nothing ->
                    attributes
    in
    div []
        (List.append
            [ input
                (activeDescendant
                    [ type_ "text"
                    , onInput (MsgAutocom model.fieldId << SetQuery)
                    , onFocus (MsgAutocom model.fieldId OnFocusAutocom)
                    , onBlur (MsgAutocom model.fieldId OnBlurAutocom)
                    , onWithOptions "keydown" options dec
                    , value fieldValue
                    , id model.fieldId
                    , classList
                        [ ( "autocomplete-input", True )
                        ]
                    , autocomplete False
                    , attribute "aria-owns" "list-of-presidents"
                    , attribute "aria-expanded" <| String.toLower <| toString model.showMenu
                    , attribute "aria-haspopup" <| String.toLower <| toString model.showMenu
                    , attribute "role" "combobox"
                    , attribute "aria-autocomplete" "list"
                    ]
                )
                []
            ]
            menu
        )


acceptableItems : String -> List MenuItem -> List MenuItem
acceptableItems fieldValue menuItems =
    let
        lowerQuery =
            String.toLower fieldValue
    in
    List.filter (String.contains lowerQuery << String.toLower) menuItems


viewMenu : AutocompleteModel -> Html Msg
viewMenu model =
    -- This is the menu that appear below the input box
    div [ class "autocomplete-menu", onMouseDown (MsgAutocom model.fieldId PreventFocusAndBlur) ]
        [ Html.map (MsgAutocom model.fieldId << SetAutoState) (Autocomplete.view viewConfig model.howManyToShow model.state (acceptableItems model.fieldValue model.menuItems)) ]


updateConfig : String -> Autocomplete.UpdateConfig Msg MenuItem
updateConfig msgId =
    Autocomplete.updateConfig
        { toId = \id -> id
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map (MsgAutocom msgId << PreviewMenuItem) maybeId
                else if code == 13 then
                    Maybe.map (MsgAutocom msgId << SelectMenuItemKeyboard) maybeId
                else
                    Just <| MsgAutocom msgId Reset
        , onTooLow = Just <| (MsgAutocom msgId << Wrap) False
        , onTooHigh = Just <| (MsgAutocom msgId << Wrap) True

        -- lucamug - Removed this to avoid the input field changing on mouse
        -- over
        -- , onMouseEnter = \id -> Just <| (MsgAutocom << PreviewMenuItem) id
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| (MsgAutocom msgId << SelectMenuItemMouse) id
        , separateSelections = False
        }


viewConfig : Autocomplete.ViewConfig MenuItem
viewConfig =
    let
        customizedLi keySelected mouseSelected menuItem =
            { attributes =
                [ classList [ ( "autocomplete-item", True ), ( "key-selected", keySelected || mouseSelected ) ]
                , id menuItem
                ]
            , children = [ Html.text menuItem ]
            }
    in
    Autocomplete.viewConfig
        { toId = \id -> id
        , ul = [ class "autocomplete-list" ]
        , li = customizedLi
        }



-- MENU ITEMS


type alias MenuItem =
    String


menuItems1 : List MenuItem
menuItems1 =
    [ "Alabama"
    , "Alaska"
    , "Arizona"
    , "Arkansas"
    , "California"
    , "Colorado"
    , "Connecticut"
    , "Delaware"
    , "Florida"
    , "Georgia"
    , "Hawaii"
    , "Idaho"
    , "Illinois"
    , "Indiana"
    , "Iowa"
    , "Kansas"
    , "Kentucky"
    , "Louisiana"
    , "Maine"
    , "Maryland"
    , "Massachusetts"
    , "Michigan"
    , "Minnesota"
    , "Mississippi"
    , "Missouri"
    , "Montana"
    , "Nebraska"
    , "Nevada"
    , "New Hampshire"
    , "New Jersey"
    , "New Mexico"
    , "New York"
    , "North Carolina"
    , "North Dakota"
    , "Ohio"
    , "Oklahoma"
    , "Oregon"
    , "Pennsylvania"
    , "Rhode Island"
    , "South Carolina"
    , "South Dakota"
    , "Tennessee"
    , "Texas"
    , "Utah"
    , "Vermont"
    , "Virginia"
    , "Washington"
    , "West Virginia"
    , "Wisconsin"
    , "Wyoming"
    ]


menuItems2 : List MenuItem
menuItems2 =
    [ "ABAP"
    , "ABC"
    , "ActionScript"
    , "Ada"
    , "Agilent VEE"
    , "Algol"
    , "Alice"
    , "Angelscript"
    , "Apex"
    , "APL"
    , "AppleScript"
    , "Arc"
    , "Arduino"
    , "ASP"
    , "AspectJ"
    , "Assembly"
    , "ATLAS"
    , "Augeas"
    , "AutoHotkey"
    , "AutoIt"
    , "AutoLISP"
    , "Automator"
    , "Avenue"
    , "Awk"
    , "Elm"
    ]

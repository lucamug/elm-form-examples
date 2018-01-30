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
    , autocomFieldValue : String
    , autocomMenuItems : List MenuItem
    , autocomState : Autocomplete.State
    , autocomHowManyToShow : Int
    , autocomSelectedMenuItem : Maybe MenuItem
    , autocomShowMenu : Bool
    , autocomFocus : Bool
    , autocomPreventingBlur : Bool
    , autocomPreventingFocus : Bool

    -- AUTOCOMPLETE 2
    , autocomplete2 :
        { autocom2FieldValue : String
        , autocom2MenuItems : List MenuItem
        , autocom2State : Autocomplete.State
        , autocom2HowManyToShow : Int
        , autocom2SelectedMenuItem : Maybe MenuItem
        , autocom2ShowMenu : Bool
        , autocom2Focus : Bool
        , autocom2PreventingBlur : Bool
        , autocom2PreventingFocus : Bool
        }
    }


init : ( Model, Cmd msg )
init =
    ( { -- FOCUS
        focus = Nothing

      -- FIELDS VALUES
      , field1 = ""
      , field2 = ""

      -- AUTOCOMPLETE 1
      , autocomFieldValue = ""
      , autocomMenuItems = menuItems1
      , autocomState = Autocomplete.empty
      , autocomHowManyToShow = 50
      , autocomSelectedMenuItem = Nothing
      , autocomShowMenu = False
      , autocomFocus = False
      , autocomPreventingBlur = False
      , autocomPreventingFocus = False

      -- AUTOCOMPLETE 2
      , autocomplete2 =
            { autocom2FieldValue = ""
            , autocom2MenuItems = menuItems2
            , autocom2State = Autocomplete.empty
            , autocom2HowManyToShow = 50
            , autocom2SelectedMenuItem = Nothing
            , autocom2ShowMenu = False
            , autocom2Focus = False
            , autocom2PreventingBlur = False
            , autocom2PreventingFocus = False
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
    | MsgAutocom MsgAutocom


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
        MsgAutocom autocomMsg ->
            updateAutocom autocomMsg model



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
            , label []
                [ h2 [] [ text "#3 Using elm-autocomplete" ]
                , ul []
                    [ li [] [ text <| "Focus: " ++ toString model.autocomFocus ]
                    , li [] [ text <| "Value: " ++ model.autocomFieldValue ]
                    , li [] [ text <| "State:" ++ toString model.autocomState ]
                    , li [] [ text <| "HowMany:" ++ toString model.autocomHowManyToShow ]
                    , li [] [ text <| "Selected:" ++ toString model.autocomSelectedMenuItem ]
                    , li [] [ text <| "ShowMenu:" ++ toString model.autocomShowMenu ]
                    ]
                , viewAutocom model
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map (MsgAutocom << SetAutoState) Autocomplete.subscription



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


updateAutocom : MsgAutocom -> Model -> ( Model, Cmd Msg )
updateAutocom msg model =
    case msg of
        SetQuery newQuery ->
            let
                autocomShowMenu =
                    not << List.isEmpty <| acceptableItems newQuery model.autocomMenuItems
            in
            { model | autocomFieldValue = newQuery, autocomShowMenu = autocomShowMenu, autocomSelectedMenuItem = Nothing } ! []

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg model.autocomHowManyToShow model.autocomState (acceptableItems model.autocomFieldValue model.autocomMenuItems)

                newModel =
                    { model | autocomState = newState }
            in
            case maybeMsg of
                Nothing ->
                    newModel ! []

                Just updateMsg ->
                    update updateMsg newModel

        Wrap toTop ->
            case model.autocomSelectedMenuItem of
                Just menuItem ->
                    update (MsgAutocom Reset) model

                Nothing ->
                    if toTop then
                        { model
                            | autocomState = Autocomplete.resetToLastItem updateConfig (acceptableItems model.autocomFieldValue model.autocomMenuItems) model.autocomHowManyToShow model.autocomState
                            , autocomSelectedMenuItem = List.head <| List.reverse <| List.take model.autocomHowManyToShow <| acceptableItems model.autocomFieldValue model.autocomMenuItems
                        }
                            ! []
                    else
                        { model
                            | autocomState = Autocomplete.resetToFirstItem updateConfig (acceptableItems model.autocomFieldValue model.autocomMenuItems) model.autocomHowManyToShow model.autocomState
                            , autocomSelectedMenuItem = List.head <| List.take model.autocomHowManyToShow <| acceptableItems model.autocomFieldValue model.autocomMenuItems
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
            in
            -- Giving the focus again (Dom.focus) because it was lost upon
            -- the click on the menu
            ( newModel, Task.attempt (\_ -> MsgAutocom NoOpAutocom) (Dom.focus "president-input") )

        PreviewMenuItem id ->
            { model | autocomSelectedMenuItem = Just <| getMenuItemAtId model.autocomMenuItems id } ! []

        HandleEscape ->
            let
                validOptions =
                    not <| List.isEmpty (acceptableItems model.autocomFieldValue model.autocomMenuItems)

                handleEscape =
                    if validOptions then
                        model
                            |> removeSelection
                            |> resetMenu
                    else
                        model
                            |> resetInput

                escapedModel =
                    case model.autocomSelectedMenuItem of
                        Just menuItem ->
                            if model.autocomFieldValue == menuItem then
                                model
                                    |> resetInput
                            else
                                handleEscape

                        Nothing ->
                            handleEscape
            in
            escapedModel ! []

        Reset ->
            { model
                | autocomState = Autocomplete.reset updateConfig model.autocomState
                , autocomSelectedMenuItem = Nothing
            }
                ! []

        OnFocusAutocom ->
            -- lucamug -
            if model.autocomPreventingFocus then
                ( { model | autocomPreventingFocus = False }, Cmd.none )
            else
                { model | autocomFocus = True, autocomShowMenu = True } ! []

        OnBlurAutocom ->
            -- lucamug - Closing the drop down on Blur. Not sure if resetMenu is the right
            -- thing to do here. Maybe not, I just want to close the menu.
            -- ({ model | autocomFocus = False } |> resetMenu) ! []
            if model.autocomPreventingBlur then
                ( { model | autocomPreventingBlur = False }, Cmd.none )
            else
                { model | autocomFocus = False, autocomShowMenu = False } ! []

        PreventFocusAndBlur ->
            ( { model | autocomPreventingFocus = True, autocomPreventingBlur = True }, Cmd.none )

        NoOpAutocom ->
            model ! []


resetInput : Model -> Model
resetInput model =
    { model | autocomFieldValue = "" }
        |> removeSelection
        |> resetMenu


resetMenu : Model -> Model
resetMenu model =
    { model
        | autocomState = Autocomplete.empty
        , autocomShowMenu = False
    }


removeSelection : Model -> Model
removeSelection model =
    { model | autocomSelectedMenuItem = Nothing }


getMenuItemAtId : List MenuItem -> String -> MenuItem
getMenuItemAtId autocomMenuItems id =
    List.filter (\menuItem -> menuItem == id) autocomMenuItems
        |> List.head
        |> Maybe.withDefault ""


setQuery : Model -> String -> Model
setQuery model id =
    { model
        | autocomFieldValue = getMenuItemAtId model.autocomMenuItems id
        , autocomSelectedMenuItem = Just <| getMenuItemAtId model.autocomMenuItems id
    }


viewAutocom : Model -> Html Msg
viewAutocom model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            Decode.map
                (\code ->
                    if code == 38 || code == 40 then
                        Ok (MsgAutocom NoOpAutocom)
                    else if code == 27 then
                        Ok (MsgAutocom HandleEscape)
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
            if model.autocomShowMenu then
                [ viewMenu model ]
            else
                []

        autocomFieldValue =
            case model.autocomSelectedMenuItem of
                Just menuItem ->
                    menuItem

                Nothing ->
                    model.autocomFieldValue

        activeDescendant attributes =
            case model.autocomSelectedMenuItem of
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
                    , onInput (MsgAutocom << SetQuery)
                    , onFocus (MsgAutocom OnFocusAutocom)
                    , onBlur (MsgAutocom OnBlurAutocom)
                    , onWithOptions "keydown" options dec
                    , value autocomFieldValue
                    , id "president-input"
                    , classList
                        [ ( "autocomplete-input", True )
                        ]
                    , autocomplete False
                    , attribute "aria-owns" "list-of-presidents"
                    , attribute "aria-expanded" <| String.toLower <| toString model.autocomShowMenu
                    , attribute "aria-haspopup" <| String.toLower <| toString model.autocomShowMenu
                    , attribute "role" "combobox"
                    , attribute "aria-autocomplete" "list"
                    ]
                )
                []
            ]
            menu
        )


acceptableItems : String -> List MenuItem -> List MenuItem
acceptableItems autocomFieldValue autocomMenuItems =
    let
        lowerQuery =
            String.toLower autocomFieldValue
    in
    List.filter (String.contains lowerQuery << String.toLower) autocomMenuItems


viewMenu : Model -> Html Msg
viewMenu model =
    -- This is the menu that appear below the input box
    div [ class "autocomplete-menu", onMouseDown (MsgAutocom PreventFocusAndBlur) ]
        [ Html.map (MsgAutocom << SetAutoState) (Autocomplete.view viewConfig model.autocomHowManyToShow model.autocomState (acceptableItems model.autocomFieldValue model.autocomMenuItems)) ]


updateConfig : Autocomplete.UpdateConfig Msg MenuItem
updateConfig =
    Autocomplete.updateConfig
        { toId = \id -> id
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map (MsgAutocom << PreviewMenuItem) maybeId
                else if code == 13 then
                    Maybe.map (MsgAutocom << SelectMenuItemKeyboard) maybeId
                else
                    Just <| MsgAutocom Reset
        , onTooLow = Just <| (MsgAutocom << Wrap) False
        , onTooHigh = Just <| (MsgAutocom << Wrap) True

        -- lucamug - Removed this to avoud the input field changing on mouse
        -- over
        -- , onMouseEnter = \id -> Just <| (MsgAutocom << PreviewMenuItem) id
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| (MsgAutocom << SelectMenuItemMouse) id
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

module Main exposing (main)

import Autocomplete
import AutocompleteWrapper
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { field1 : AutocompleteWrapper.Model
    , field2 : AutocompleteWrapper.Model
    , field3 : AutocompleteWrapper.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { field1 = AutocompleteWrapper.init "id1" menuItems1
      , field2 = AutocompleteWrapper.init "id2" menuItems2
      , field3 = AutocompleteWrapper.init "id3" menuItems3
      }
    , Cmd.none
    )


type Msg
    = OriginMessage String AutocompleteWrapper.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        msgConverter : Msg -> ( String, AutocompleteWrapper.Msg )
        msgConverter msg =
            case msg of
                OriginMessage fieldId autocomMsg ->
                    ( fieldId, autocomMsg )

        ( fieldId, autocomMsg ) =
            msgConverter msg
    in
    if fieldId == model.field1.fieldId then
        let
            ( newModel, newMsg ) =
                AutocompleteWrapper.update autocomMsg model.field1 OriginMessage msgConverter
        in
        { model | field1 = newModel } ! [ newMsg ]
    else if fieldId == model.field2.fieldId then
        let
            ( newModel, newMsg ) =
                AutocompleteWrapper.update autocomMsg model.field2 OriginMessage msgConverter
        in
        { model | field2 = newModel } ! [ newMsg ]
    else if fieldId == model.field3.fieldId then
        let
            ( newModel, newMsg ) =
                AutocompleteWrapper.update autocomMsg model.field3 OriginMessage msgConverter
        in
        { model | field3 = newModel } ! [ newMsg ]
    else
        model ! []



-- VIEWS


view : Model -> Html Msg
view model =
    div [ class "form-container" ]
        [ div
            []
            [ label []
                [ text "State"
                , AutocompleteWrapper.view model.field1 OriginMessage
                ]
            , label []
                [ text "Programming Language"
                , AutocompleteWrapper.view model.field2 OriginMessage
                ]
            , label []
                [ text "Color"
                , AutocompleteWrapper.view model.field3 OriginMessage
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map (OriginMessage model.field1.fieldId << AutocompleteWrapper.SetAutoState) Autocomplete.subscription
        , Sub.map (OriginMessage model.field2.fieldId << AutocompleteWrapper.SetAutoState) Autocomplete.subscription
        , Sub.map (OriginMessage model.field3.fieldId << AutocompleteWrapper.SetAutoState) Autocomplete.subscription
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



-- MENU ITEMS


menuItems1 : List AutocompleteWrapper.MenuItem
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


menuItems2 : List AutocompleteWrapper.MenuItem
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


menuItems3 : List AutocompleteWrapper.MenuItem
menuItems3 =
    [ "Blue"
    , "Yellow"
    , "Red"
    , "Green"
    ]

module Main exposing (main)

import Date
import DatePicker
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Svg
import Svg.Attributes as SA
import Utils
import Validate


exampleVersion : String
exampleVersion =
    "20"


type alias Model =
    { errors : List Error
    , email : String
    , password : String
    , fruits : Dict.Dict String Bool
    , response : Maybe String
    , focus : Maybe FormField
    , showErrors : Bool
    , showPassword : Bool
    , formState : FormState
    , date : Maybe Date.Date
    , datePicker : DatePicker.DatePicker
    , defaultSettings : DatePicker.Settings
    }


init : ( Model, Cmd Msg )
init =
    let
        isDisabled date =
            Date.dayOfWeek date
                |> flip List.member [ Date.Sat, Date.Sun ]

        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { errors = []
      , email = ""
      , password = ""
      , fruits =
            Dict.fromList
                [ ( "Apple", False )
                , ( "Banana", False )
                , ( "Orange", False )
                , ( "Pear", False )
                , ( "Strawberry", False )
                , ( "Cherry", False )
                , ( "Grapes", False )
                , ( "Watermelon", False )
                , ( "Pineapple", False )
                ]
      , response = Nothing
      , focus = Nothing
      , showErrors = False
      , showPassword = False
      , formState = Editing
      , date = Nothing
      , datePicker = datePicker
      , defaultSettings = DatePicker.defaultSettings
      }
    , Cmd.map ToDatePicker datePickerFx
    )


type alias Error =
    ( FormField, String )


type alias Fruit =
    String


type FormState
    = Editing
    | Fetching


type Msg
    = NoOp
    | SubmitForm
    | SetField FormField String
    | Response (Result Http.Error String)
    | OnFocus FormField
    | OnBlur FormField
    | ToggleShowPasssword
    | ToggleFruit Fruit
    | ToDatePicker DatePicker.Msg


type FormField
    = Email
    | Password


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        SubmitForm ->
            case validate model of
                [] ->
                    ( { model | errors = [], response = Nothing, formState = Fetching }
                    , Http.send Response (postRequest model)
                    )

                errors ->
                    ( { model | errors = errors, showErrors = True }
                    , Cmd.none
                    )

        SetField field value ->
            ( model
                |> setField field value
                |> setErrors
            , Cmd.none
            )

        Response (Ok response) ->
            ( { model | response = Just response, formState = Editing }, Cmd.none )

        Response (Err error) ->
            ( { model | response = Just (toString error), formState = Editing }, Cmd.none )

        OnFocus formField ->
            ( { model | focus = Just formField }, Cmd.none )

        OnBlur formField ->
            ( { model | focus = Nothing }, Cmd.none )

        ToggleShowPasssword ->
            ( { model | showPassword = not model.showPassword }, Cmd.none )

        ToggleFruit fruit ->
            ( { model | fruits = toggle fruit model.fruits }, Cmd.none )

        ToDatePicker msg ->
            let
                ( newDatePicker, _, mDate ) =
                    DatePicker.update settings msg model.datePicker

                date =
                    case mDate of
                        DatePicker.Changed date ->
                            date

                        _ ->
                            model.date
            in
            ( { model
                | date = date
                , datePicker = newDatePicker
              }
            , Cmd.none
            )



-- HELPERS


settings : DatePicker.Settings
settings =
    let
        defaultSettings =
            DatePicker.defaultSettings

        isDisabled date =
            Date.dayOfWeek date
                |> flip List.member [ Date.Sat, Date.Sun ]
    in
    { defaultSettings
        | isDisabled = isDisabled
        , placeholder = ""
    }


toggle : comparable -> Dict.Dict comparable Bool -> Dict.Dict comparable Bool
toggle key dict =
    Dict.update key
        (\oldValue ->
            case oldValue of
                Just value ->
                    Just <| not value

                Nothing ->
                    Nothing
        )
        dict


setErrors : Model -> Model
setErrors model =
    case validate model of
        [] ->
            { model | errors = [] }

        errors ->
            { model | errors = errors }


setField : FormField -> String -> Model -> Model
setField field value model =
    case field of
        Email ->
            { model | email = value }

        Password ->
            { model | password = value }


filteredFruits : Dict.Dict comparable Bool -> List comparable
filteredFruits fruits =
    Dict.keys
        (Dict.filter (\key value -> value) fruits)


maxFruitSelectable : Int
maxFruitSelectable =
    3


fruitsQuantityHaveReachedTheLimit : Dict.Dict comparable Bool -> Bool
fruitsQuantityHaveReachedTheLimit fruits =
    List.length (filteredFruits fruits) >= maxFruitSelectable


postRequest : Model -> Http.Request String
postRequest model =
    let
        body =
            Encode.object
                [ ( "email", Encode.string model.email )
                , ( "password", Encode.string model.password )
                , ( "fruits"
                  , Encode.list <|
                        List.map (\key -> Encode.string key)
                            (filteredFruits model.fruits)
                  )
                ]
                |> Http.jsonBody
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = Utils.urlMirrorService
        , body = body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


validate : Model -> List Error
validate =
    Validate.all
        [ .email >> Validate.ifBlank ( Email, "Email can't be blank." )
        , .password >> Validate.ifBlank ( Password, "Password can't be blank." )
        ]


onEnter : msg -> Attribute msg
onEnter msg =
    keyCode
        |> Decode.andThen
            (\key ->
                if key == 13 then
                    Decode.succeed msg
                else
                    Decode.fail "Not enter"
            )
        |> on "keyup"


formatDate : Date.Date -> String
formatDate d =
    toString (Date.month d) ++ " " ++ toString (Date.day d) ++ ", " ++ toString (Date.year d)



-- VIEWS


view : Model -> Html Msg
view model =
    Utils.viewUtils model exampleVersion viewForm


viewInput : Model -> FormField -> String -> String -> Html Msg
viewInput model formField inputType inputName =
    let
        hasFocus =
            case model.focus of
                Just focusedField ->
                    focusedField == formField

                Nothing ->
                    False

        content =
            case formField of
                Email ->
                    model.email

                Password ->
                    model.password
    in
    label
        []
        [ div [ class "inputFieldContainer" ]
            [ input
                [ if formField == Password && model.showPassword then
                    type_ "text"
                  else
                    type_ inputType
                , classList [ ( "focus", hasFocus ) ]
                , onInput <| SetField formField
                , onFocus <| OnFocus formField
                , onBlur <| OnBlur formField
                , value content
                ]
                []
            , div
                [ classList
                    [ ( "placeholder", True )
                    , ( "upperPosition", hasFocus || content /= "" )
                    ]
                ]
                [ text inputName ]
            , if formField == Password then
                div [ class "iconInsideField", onClick ToggleShowPasssword ]
                    [ if model.showPassword then
                        svgHide "orange"
                      else
                        svgShow "orange"
                    ]
              else
                text ""
            ]
        , viewFormErrors model formField model.errors
        ]


viewForm : Model -> Html Msg
viewForm model =
    div [ class "form-container" ]
        [ div
            [ onEnter SubmitForm
            ]
            [ node "style" [] [ text "" ]
            , viewInput model Email "text" "Email"
            , viewInput model Password "password" "Password"
            , label []
                [ div [ class "inputFieldContainer" ]
                    [ DatePicker.view model.date settings model.datePicker
                        |> Html.map ToDatePicker
                    , div
                        [ classList
                            [ ( "placeholder", True )
                            , ( "upperPosition"
                              , not (model.date == Nothing)
                                    || DatePicker.isOpen model.datePicker
                              )
                            ]
                        ]
                        [ text
                            (case model.date of
                                Nothing ->
                                    "Date"

                                Just date ->
                                    "Date: " ++ formatDate date
                            )
                        ]
                    ]
                ]
            , label []
                [ div [ class "inputFieldContainer" ]
                    [ input
                        [ type_ "date"
                        ]
                        []
                    , div
                        [ classList
                            [ ( "placeholder", True )
                            , ( "upperPosition", True )
                            ]
                        ]
                        [ a [ href "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/date" ] [ text "HTML5 Date" ]
                        ]
                    ]
                ]
            , div [ class "checkboxContainer" ]
                (List.map
                    (\fruit ->
                        let
                            value =
                                Dict.get fruit model.fruits

                            isDisabled =
                                fruitsQuantityHaveReachedTheLimit model.fruits && not (Maybe.withDefault False value)

                            isChecked =
                                Maybe.withDefault False value
                        in
                        label
                            [ classList
                                [ ( "checkbox", True )
                                , ( "disabled", isDisabled )
                                , ( "checked", isChecked )
                                ]
                            ]
                            [ input
                                [ type_ "checkbox"
                                , checked isChecked
                                , disabled isDisabled
                                , onClick <| ToggleFruit fruit
                                ]
                                []
                            , text <| " " ++ fruit
                            , viewSvgFor fruit
                            ]
                    )
                    (Dict.keys
                        model.fruits
                    )
                )
            , div [ class "formMessage" ]
                [ text <|
                    "Select max "
                        ++ toString maxFruitSelectable
                        ++ " fruits - Selected: "
                        ++ toString (List.length <| filteredFruits model.fruits)
                ]
            , button
                [ onClick SubmitForm
                , classList
                    [ ( "disabled", not (List.isEmpty model.errors) && model.showErrors ) ]
                ]
                [ text "Submit" ]
            ]
        , if model.formState == Fetching then
            div [ class "form-cover" ] []
          else
            text ""
        ]


viewSvgFor : String -> Html msg
viewSvgFor fruit =
    div
        [ class "svgContainer" ]
        [ case fruit of
            "Apple" ->
                svgApple

            "Banana" ->
                svgBanana

            "Orange" ->
                svgOrange

            "Pear" ->
                svgPear

            "Strawberry" ->
                svgStrawberry

            "Cherry" ->
                svgCherry

            "Grapes" ->
                svgGrapes

            "Watermelon" ->
                svgWatermelon

            "Pineapple" ->
                svgPineapple

            _ ->
                text ""
        ]


viewFormErrors : Model -> FormField -> List Error -> Html msg
viewFormErrors model field errors =
    if model.showErrors then
        errors
            |> List.filter (\( fieldError, _ ) -> fieldError == field)
            |> List.map (\( _, error ) -> li [] [ text error ])
            |> ul [ class "formErrors" ]
    else
        ul [ class "formErrors" ] []



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- SVG ICONS


svgHide : String -> Html msg
svgHide color =
    Svg.svg [ SA.viewBox "0 0 512 512", SA.height "32", SA.width "32" ]
        [ Svg.path
            [ SA.fill
                color
            , SA.d
                "M506 241l-89-89-14-13-258 258a227 227 0 0 0 272-37l89-89c8-8 8-22 0-30zM256 363a21 21 0 0 1 0-43c35 0 64-29 64-64a21 21 0 0 1 43 0c0 59-48 107-107 107zM95 152L6 241c-8 8-8 22 0 30l89 89 14 13 258-258c-86-49-198-37-272 37zm161 40c-35 0-64 29-64 64a21 21 0 0 1-43 0c0-59 48-107 107-107a21 21 0 0 1 0 43z"
            ]
            []
        ]


svgShow : String -> Html msg
svgShow color =
    Svg.svg [ SA.viewBox "0 0 512 512", SA.height "32", SA.width "32" ]
        [ Svg.path
            [ SA.fill
                color
            , SA.d
                "M256 192a64 64 0 1 0 0 128 64 64 0 0 0 0-128zm250 49l-89-89c-89-89-233-89-322 0L6 241c-8 8-8 22 0 30l89 89a227 227 0 0 0 322 0l89-89c8-8 8-22 0-30zM256 363a107 107 0 1 1 0-214 107 107 0 0 1 0 214z"
            ]
            []
        ]


svgCherry : Html.Html msg
svgCherry =
    Svg.svg [ SA.viewBox "0 0 57.6 57.6" ]
        [ Svg.path [ SA.fill "#d13834", SA.d "M2.8 37.6c0-7.7 5.4-14 12-14h2c6.6 0 12 6.3 12 14a13 13 0 1 1-26 0z" ] []
        , Svg.path [ SA.fill "#ed3f32", SA.d "M13.8 47.4h-.3C10 46.6 7.1 44 6 40.6a1 1 0 0 1 1.9-.6 8.4 8.4 0 0 0 6 5.4 1 1 0 0 1-.1 2zM6.7 36.2h-.2a1 1 0 0 1-.7-1.2 11 11 0 0 1 1.4-3.8 1 1 0 0 1 1.7 1c-.5 1-1 2-1.2 3.2a1 1 0 0 1-1 .8zm22.1 8.4c0-7.7 5.4-14 12-14h2c6.6 0 12 6.3 12 14a13 13 0 1 1-26 0zm-5-11a6 6 0 0 1-6-6 1 1 0 0 1 2 0 4 4 0 0 0 4 4 1 1 0 0 1 0 2z" ] []
        , Svg.path [ SA.fill "#ed7161", SA.d "M47.8 40.6a6 6 0 0 1-6-6 1 1 0 0 1 2 0 4 4 0 0 0 4 4 1 1 0 0 1 0 2z" ] []
        , Svg.path [ SA.fill "#4c312c", SA.d "M45.8 36.6a1 1 0 0 1-.9-1.5c4.3-7.6 3.6-14-3-24.4-1.5 11-11.3 19.9-19.5 19.9a1 1 0 0 1 0-2c7.8 0 17.8-9.9 17.8-21a1 1 0 0 1 1.8-.5c7.2 10.7 10.3 19 4.7 29a1 1 0 0 1-.9.5z" ] []
        , Svg.path [ SA.fill "#88c057", SA.d "M40.8 7c4 3 10.2 1.5 13.1-2.5-4-3-10.2-1.4-13 2.6z" ] []
        , Svg.path [ SA.fill "#659c35", SA.d "M29.8.1c.8 5 6.2 8.2 11.1 7.4-.7-5-6.2-8.1-11-7.4z" ] []
        , Svg.path [ SA.fill "#ed7161", SA.d "M39.6 54.7h-.2c-3.6-.8-6.4-3.4-7.5-6.8a1 1 0 0 1 1.9-.6 8.4 8.4 0 0 0 6 5.5 1 1 0 0 1-.2 2z" ] []
        ]


svgGrapes : Html.Html msg
svgGrapes =
    Svg.svg [ SA.viewBox "0 0 56 56" ]
        [ Svg.circle [ SA.cx "6", SA.cy "49", SA.r "6", SA.fill "#9777a8" ] []
        , Svg.circle [ SA.cx "10", SA.cy "28", SA.r "6", SA.fill "#583e68" ] []
        , Svg.path [ SA.fill "#583e68", SA.d "M16 41a6 6 0 0 0-5.6 4 6 6 0 0 1 1.2 6A6 6 0 0 0 22 47a6 6 0 0 0-6-6z" ] []
        , Svg.path [ SA.fill "#6f58a8", SA.d "M10.4 45a6 6 0 0 1 3.4-3.6L14 40a6 6 0 1 0-10.7 3.7 6 6 0 0 1 7 1.2zM26 38a6 6 0 0 0-5.8 4.7 6 6 0 0 1 1.6 5.6A6 6 0 0 0 32 44a6 6 0 0 0-6-6z" ] []
        , Svg.path [ SA.fill "#9777a8", SA.d "M23.4 38.6a6 6 0 0 0-8-8 6 6 0 0 1-2.8 2.8 6 6 0 0 0-.6 2.2 6 6 0 0 1 2 4.4v.4l.9.7a6 6 0 0 1 4.2.8 6 6 0 0 0 1.5-.5 6 6 0 0 1 2.8-2.8z" ] []
        , Svg.path [ SA.fill "#583e68", SA.d "M28 28a6 6 0 0 0-2.2.4 6 6 0 0 1-3 4 6 6 0 0 1 .8 5.7l.3.3a6 6 0 0 1 6.2 1.2A6 6 0 0 0 28 28z" ] []
        , Svg.path [ SA.fill "#9777a8", SA.d "M36 34a6 6 0 0 0-2 .4 6 6 0 0 1-4 5.2 6 6 0 0 1 2 4.4v.4A6 6 0 1 0 36 34z" ] []
        , Svg.path [ SA.fill "#583e68", SA.d "M38 24a6 6 0 0 0-1.1.1A6 6 0 0 1 32 29v.7a6 6 0 0 1 2 4.3v.4a8 8 0 0 1 2-.4 6 6 0 0 1 4 1.6A6 6 0 0 0 38 24z" ] []
        , Svg.path [ SA.fill "#9777a8", SA.d "M25 11a6 6 0 0 0-5.5 3.6 6 6 0 0 1 0 4.8 6 6 0 0 0 1 1.6 6 6 0 0 1 4 2h.5a6 6 0 0 1 6-6 6 6 0 0 0-6-6z" ] []
        , Svg.path [ SA.fill "#6f58a8", SA.d "M14 11a6 6 0 0 0-3.2 11 6 6 0 0 1 2.4 1h.8a6 6 0 1 0 0-12z" ] []
        , Svg.path [ SA.fill "#6f58a8", SA.d "M25 23.8a6 6 0 0 0-6.8-2.5 6 6 0 0 1-2.4 1.4 6 6 0 0 0-1.1 1.5 6 6 0 0 1 .7 6.4 6 6 0 0 1 7.3 1.7 6 6 0 0 0 3.2-6 6 6 0 0 1-.8-2.5z" ] []
        , Svg.path [ SA.fill "#6f58a8", SA.d "M31 17a6 6 0 0 0-3.3 11h.3a6 6 0 0 1 3.3 1 6 6 0 0 0-.3-12z" ] []
        , Svg.path [ SA.fill "#88c057", SA.d "M56 26a12 12 0 0 1-12-12 12 12 0 0 1 12 12z" ] []
        , Svg.path [ SA.fill "#7a3726", SA.d "M34 19v-2c7.2 0 13-7.2 13-16h2c0 10-6.7 18-15 18z" ] []
        ]


svgWatermelon : Html.Html msg
svgWatermelon =
    Svg.svg [ SA.viewBox "0 0 50.5 50.5" ]
        [ Svg.path [ SA.fill "#88c057", SA.d "M18.4 24l1.4 4.5c.2 1.1-.6 2.2-1.7 2.6l-1.1.4-.3 1a4 4 0 0 1-2.7 2.7c-1.2.3-3.7-2.3-4.7-2.1L0 42.4A30 30 0 0 0 42.4 0l-24 24z" ] []
        , Svg.path [ SA.fill "#e22f37", SA.d "M37 5.3L18.5 24l1.4 4.5c.2 1.1-.6 2.2-1.7 2.6l-1.1.4-.3 1a4 4 0 0 1-2.7 2.7c-1.2.3-3.7-2.3-4.7-2.1l-4 4c8.8 9.5 22.4 8.7 31.5-.3S46.6 14 37 5.3z" ] []
        , Svg.circle [ SA.cx "4.5", SA.cy "17", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "16.5", SA.cy "39", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "26", SA.cy "25.6", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "30.9", SA.cy "20.7", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "28.1", SA.cy "37.6", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "33", SA.cy "32.7", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "38", SA.cy "27.7", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "35.9", SA.cy "15.7", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "22.4", SA.cy "36.2", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "27.5", SA.cy "31", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "32.5", SA.cy "26", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "7.5", SA.cy "27", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "13.5", SA.cy "19", SA.r "1.5", SA.fill "#231f20" ] []
        , Svg.circle [ SA.cx "37.3", SA.cy "21.4", SA.r "1.5", SA.fill "#231f20" ] []
        ]


svgStrawberry : Html.Html msg
svgStrawberry =
    Svg.svg [ SA.viewBox "0 0 57 57" ]
        [ Svg.path [ SA.fill "#659c35", SA.d "M29.8 9.4l-2.9.6L24 1.5 29 0z" ] []
        , Svg.path [ SA.fill "#88c057", SA.d "M36.1 8.5a8 8 0 0 0 2.4-3.6c-5.5-2-7.2.6-7.2.6 0-1-.9-1.5-2-1.6l.5 5.5-2.9.6-1.8-5.2c-.5.2-.8.5-.8.7 0 0-1.7-2.6-7.2-.6a7.9 7.9 0 0 0 2.4 3.7c-4.4.6-8.4 2-10.5 5.8 10.3 3 13.4-1.9 13.4-1.9.6 6.8 5.8 8.7 6.6 9 .8-.3 6-2.2 6.6-9 0 0 1.2 5 11.4 1.9-2.2-3.7-6.5-5.3-10.9-6z" ] []
        , Svg.path [ SA.fill "#e22f37", SA.d "M45.3 15v-.1c-8.7 2-9.7-2.4-9.7-2.4-.6 6.8-5.8 8.7-6.6 9-.8-.3-6-2.2-6.6-9 0 0-2.6 4-10.8 2.5C9.3 17.6 8 21.4 8 27c0 13 12.8 30 20.5 30C36.2 57 49 39.9 49 27c0-5.8-1.3-9.6-3.8-12h.1z" ] []
        , Svg.path [ SA.fill "#994530", SA.d "M17.3 20.7c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 1.9 1.9c.5 0 1-.2 1.4-.6.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8zm11.8 4c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 2 1.9c.4 0 1-.2 1.3-.6.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8zm11.2-4c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 1.9 1.9c.5 0 1-.2 1.4-.6.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8zm-18.2 13c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 2 2c.4 0 1-.3 1.3-.7.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8zm7 9c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 2 1.9c.4 0 1-.2 1.3-.6.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8zm7-9c-.1-.4-.7-.4-.8 0 0 0-1.5 5.3-1.5 5.8a2 2 0 0 0 2 1.9c.4 0 1-.2 1.3-.6.3-.3.5-.8.5-1.3s-1.5-5.8-1.5-5.8z" ] []
        ]


svgOrange : Html.Html msg
svgOrange =
    Svg.svg [ SA.viewBox "0 0 51.5 51.5" ]
        [ Svg.path [ SA.fill "#ed8f20", SA.d "M36.5 12.9l-5.1-.4 1.9-4.8 1.1-2.1a24 24 0 0 0-33.6 22 24 24 0 1 0 42-15.9c-2 1-4 1.4-6.3 1.2z" ] []
        , Svg.circle [ SA.cx "38.5", SA.cy "18.5", SA.r "2", SA.fill "#ef771d" ] []
        , Svg.circle [ SA.cx "43.9", SA.cy "29.7", SA.r "2", SA.fill "#ef771d" ] []
        , Svg.circle [ SA.cx "43.5", SA.cy "23.5", SA.r "2", SA.fill "#ef771d" ] []
        , Svg.circle [ SA.cx "37.5", SA.cy "27.5", SA.r "2", SA.fill "#ef771d" ] []
        , Svg.circle [ SA.cx "38.5", SA.cy "34.5", SA.r "2", SA.fill "#ef771d" ] []
        , Svg.circle [ SA.cx "20.5", SA.cy "10.5", SA.r "2", SA.fill "#ef771d" ] []
        , Svg.circle [ SA.cx "13.5", SA.cy "13.5", SA.r "2", SA.fill "#ef771d" ] []
        , Svg.path [ SA.fill "#ed8f20", SA.d "M27.8 11.5s4.2.7 5.4 3.6" ] []
        , Svg.path [ SA.fill "#9b6026", SA.d "M33.2 16.1a1 1 0 0 1-1-.6c-.9-2.3-4.6-3-4.6-3a1 1 0 0 1 .3-2c.2 0 4.8.9 6.2 4.3a1 1 0 0 1-1 1.3z" ] []
        , Svg.path [ SA.fill "#ed8f20", SA.d "M28.4 14.5a9 9 0 0 1 3.8-2.5" ] []
        , Svg.path [ SA.fill "#9b6026", SA.d "M28.4 15.5a1 1 0 0 1-.8-1.6 11 11 0 0 1 4.2-2.9 1 1 0 0 1 .7 1.9 8.8 8.8 0 0 0-3.4 2.3 1 1 0 0 1-.7.3z" ] []
        , Svg.path [ SA.fill "#659c35", SA.d "M50.8.5L45.7 0a12.3 12.3 0 0 0-12.4 7.7l-1.9 4.8 5 .4C42 13.3 47 10.2 49 5.2L50.8.5z" ] []
        , Svg.path [ SA.fill "#88c057", SA.d "M32 11l-.6 1.5h.7c6.3-6 13-8 13.8-8.1v-.6l.5.4.1-.6-.4-.4c-.6-.1-7.6 1.8-14.1 7.9z" ] []
        ]


svgApple : Html.Html msg
svgApple =
    Svg.svg [ SA.viewBox "0 0 56.7 56.7" ]
        [ Svg.path [ SA.fill "#d13834", SA.d "M31.5 16.2c-2 .5-4.2.5-6.3 0C14.4 13.6 5.4 14.6 5.5 35c0 10.3 9.8 23.8 20 21.5 1.9-.4 3.8-.4 5.6 0 10.3 2.3 20-11.2 20-21.5.1-20.3-8.9-21.3-19.6-18.7z" ] []
        , Svg.path [ SA.fill "#f75b57", SA.d "M10.5 29.7a1 1 0 0 1-1-1c0-5.5 4-10 9-10a1 1 0 1 1 0 2c-3.8 0-7 3.6-7 8 0 .5-.4 1-1 1z" ] []
        , Svg.path [ SA.fill "#4c312c", SA.d "M28.3 20a1 1 0 0 1-1-1c-.2-4.2-1.8-9.5-6-11.7a1 1 0 1 1 1-1.8c4.9 2.6 6.8 8.7 7 13.5 0 .5-.4 1-1 1z" ] []
        , Svg.path [ SA.fill "#994530", SA.d "M28.2 23.6c-1.7 0-3.3-.7-4.5-1.9a1 1 0 1 1 1.4-1.4 4.3 4.3 0 0 0 6.1 0 1 1 0 1 1 1.4 1.4 6.2 6.2 0 0 1-4.4 1.9z" ] []
        , Svg.path [ SA.fill "#659c35", SA.d "M27.3 13l.6-4A10 10 0 0 1 36.3.6l4-.6-.6 4a10 10 0 0 1-8.3 8.4l-4 .6z" ] []
        ]


svgPear : Html.Html msg
svgPear =
    Svg.svg [ SA.viewBox "0 0 58.1 58.1" ]
        [ Svg.path [ SA.fill "#659c35", SA.d "M28.8 9a1 1 0 0 1-1-1c-.6-3-1.9-5.3-3.4-6.1a1 1 0 1 1 .9-1.8c3.1 1.6 4.2 6.2 4.5 7.6a1 1 0 0 1-1 1.2z" ] []
        , Svg.path [ SA.d "M28.7 11.6c-1 0-1.8-.3-2.5-1a1 1 0 1 1 1.4-1.4c.3.3.8.4 1.2.4.6 0 1.1-.3 1.5-.7a1 1 0 1 1 1.4 1.4c-.8.8-2 1.3-3 1.3z" ] []
        , Svg.path [ SA.fill "#d7cc56", SA.d "M29.1 5.6c2.2 0 4.4 1.2 5.5 3.3 1 1.8 1.4 4 2 5.8.9 2.7 1.5 5.5 2.4 8.2.8 2.8 2.5 5.4 3.9 8 1.2 2.2 2.7 4.4 3.5 6.8 1 3.1.8 6.6-.3 9.7-2 5.7-7.3 9.4-13.2 10.4a22 22 0 0 1-7.7 0A17 17 0 0 1 12 47.4a15 15 0 0 1-.2-9.7c.8-2.4 2.3-4.6 3.5-6.9 1.4-2.5 3-5.1 3.9-8l2.3-8c.6-2 1-4.1 2-6 1-2 3.3-3.2 5.5-3.2" ] []
        , Svg.path [ SA.fill "#e3e82a", SA.d "M24 52.6h-.3c-3-.7-5.4-2.7-6.5-5.4a1 1 0 0 1 1.8-.8c1 2.1 2.8 3.7 5.2 4.2a1 1 0 0 1-.2 2zm-6.7-10.3h-.1a1 1 0 0 1-1-1c.2-1.1.3-2.2.6-3.2a1 1 0 0 1 2 .5c-.3 1-.5 1.9-.5 2.8 0 .5-.5 1-1 1z" ] []
        ]


svgPineapple : Html.Html msg
svgPineapple =
    Svg.svg [ SA.viewBox "0 0 57.1 57.1" ]
        [ Svg.path [ SA.fill "#d6a550", SA.d "M28.8 19c16 0 15.9 8.6 15.9 19s-7.1 19-16 19h-.4c-8.8 0-16-8.4-16-19s0-19 16-19" ] []
        , Svg.path [ SA.fill "#88c057", SA.d "M32.5 13.4c.5-.7 1.1-1.3 1.7-1.8.8-4.5 6.3-8.3 10.2-9.3 0 0-3.4-2.7-7.5-2.2a9.9 9.9 0 0 0-8 5c2.9 2.3 3.5 5.8 3.6 8.3z" ] []
        , Svg.path [ SA.fill "#659c35", SA.d "M23.3 19.4c1.4-.2 3.1-.3 5-.3 0 0 .2-5-5-6-5.1-1-6 2-6 2 4.5 1 5.7 3.2 6 4.3zm11.3.2a17 17 0 0 1 9.8-6.5s-1.9-4-7-3c-5.2 1-8 9-8 9 2 0 3.7.2 5.2.5z" ] []
        , Svg.path [ SA.fill "#a4e869", SA.d "M26 14.2a6.3 6.3 0 0 1 2.4 4.8h1a22.6 22.6 0 0 1 2.7-5.1l.3-.3.1-.2C32.4 9.8 31 3.9 23.4 3c-4-.5-7 2-7 2 4 1 9.6 4.6 9.6 9z" ] []
        , Svg.path [ SA.fill "#f4c44e", SA.d "M21.4 44.6l-3.7 3.8 1.4 1.4 2.3-2.3 2.3 2.3 1.4-1.4zm3.3 8.8l1.4 1.4 2.3-2.3 2.3 2.3 1.4-1.4-3.7-3.8zm7-5l1.4 1.4 2.3-2.3 2.3 2.3 1.4-1.4-3.7-3.8zm-12.6-6.6l2.3-2.3 2.3 2.3 1.4-1.4-3.7-3.8-3.7 3.8zm9.3 1.7l2.3 2.3 1.4-1.4-3.7-3.8-3.7 3.8 1.4 1.4zm-13.9-2.9l-1.7 1.8 1.4 1.4.3-.3 2.3 2.3 1.4-1.4zm20.9-1.1l2.3 2.3 1.4-1.4-3.7-3.8-3.7 3.8 1.4 1.4zm-17.7-7.1l1.4 1.4 2.3-2.3 2.3 2.3 1.4-1.4-3.7-3.8zm10.7 3.1l2.3 2.3 1.4-1.4-3.7-3.8-3.7 3.8 1.4 1.4zm7-6.9l-3.7 3.8 1.4 1.4 2.3-2.3 2.3 2.3 1.4-1.4zm-14-5.1l2.3 2.3 1.4-1.4-3.7-3.8-3.7 3.8 1.4 1.4zm4.7 6.3l2.3-2.3 2.3 2.3 1.4-1.4-3.7-3.8-3.7 3.8zm9.3-6.3l2.3 2.3 1.4-1.4-3.7-3.8-3.7 3.8 1.4 1.4zm7.8 2l-.7-1-3.8 3.9 1.4 1.4 2.1-2.2 1.3 2 .8-.5-1-3.6zm-1 15.3l-3.6 3.6 1.5 1.4 2.5-2.5 1.3.7.5-2-2.2-1.2zm-28-16c-.4.7-.7 1.4-.9 2.2l.8.8.3-.3 2.3 2.3 1.4-1.4-3.7-3.8-.2.2zM44.7 35l-2.3-2.4-3.7 3.8 1.4 1.4 2.3-2.3 1.3 1.3 1-1V35zm-32.3-.4v1.5l.7.7 1.3-1.3 2.3 2.3 1.4-1.4-3.7-3.8-2 2z" ] []
        ]


svgLemmon : Html.Html msg
svgLemmon =
    Svg.svg [ SA.viewBox "0 0 55 55" ]
        [ Svg.path [ SA.fill "#f4c44e", SA.d "M55 31H13a21 21 0 1 0 42 0z" ] []
        , Svg.path [ SA.fill "#f9ea80", SA.d "M51 31H17a17 17 0 1 0 34 0z" ] []
        , Svg.path [ SA.fill "#f9da49", SA.d "M33 31h2v17h-2z" ] []
        , Svg.path [ SA.fill "#f9da49", SA.d "M34.7 30.3l12 12-1.4 1.4-12-12z" ] []
        , Svg.path [ SA.fill "#f9da49", SA.d "M33.3 30.3l1.4 1.4-12 12-1.4-1.4z" ] []
        , Svg.path [ SA.fill "#f9d70b", SA.d "M48 11.3l.4-.4a4 4 0 0 0 0-5.7 4 4 0 0 0-5.7 0l-.2.3c-9.1-6.2-23.1-3.8-33 6s-12.3 24-6.1 33l-.3.3c-1.5 1.6-1.5 4.1 0 5.7s4.1 1.5 5.7 0l.4-.4a23.4 23.4 0 0 0 19.6 1.2A21 21 0 0 1 13 31h36.2c2.4-7 2.1-14.1-1.2-19.7zM32.9 9c-.5 1.2-1.6 1.6-2.8 1.3.6.1 0 0-.2 0a6 6 0 0 0-.4 0h-.8c-1.1.2-2.3-.5-2.5-1.7-.2-1.1.6-2.3 1.8-2.5 1.2-.2 2.4-.1 3.6.2 1.1.2 1.6 1.7 1.3 2.7z" ] []
        ]


svgBanana : Html.Html msg
svgBanana =
    Svg.svg [ SA.viewBox "0 0 54.4 54.4" ]
        [ Svg.path [ SA.fill "#f9e280", SA.d "M51.2 9l-.5-1c-.6-1-1.3-2-2.1-2.8a27.8 27.8 0 0 1-7 24.2C30.3 41.7 19.8 45.8 12.4 46.6a35 35 0 0 1-5.5 1.7h-.3c1 .2 2.2.3 3.6.3 7.1 0 19.1-2.8 32.7-17.8A31 31 0 0 0 51.2 9zM3.7 47.4v.2l.6.2-.6-.4z" ] []
        , Svg.path [ SA.fill "#e8c52e", SA.d "M48.4 5l-3-5L43 1.4l2.7 5c.2.8.5 1.5.5 2.2 0 10.5-8.4 21.5-20.7 30.2-5 3.5-8.9 6-13 7.7h-.1c7.4-.7 18-4.8 29-17a27.8 27.8 0 0 0 7-24.3v-.3z" ] []
        , Svg.path [ SA.fill "#f9d70b", SA.d "M9.6 47.6zM51.1 8.9a31 31 0 0 1-8 21.9c-13.7 15-25.7 17.8-32.8 17.8-1.4 0-2.6-.1-3.6-.3h.2c-1 .3-2 0-2.6-.5-.4 0-.6-.2-.6-.2v-.2c-.9-.7-1.5-1.6-1.8-1.6-.7 0-1.2 1.5-1.3 2.5 8.7 9.2 21.7 7.1 35.5-1C51.2 38.6 57.8 21.6 51 9z" ] []
        , Svg.path [ SA.fill "#b58c30", SA.d "M47.3 8.3l1.6-.4c.7-.2 1-1 .6-1.6a21.1 21.1 0 0 0-1.1-1.4l-2.5-4a1 1 0 0 0-1.4-.3l-.6.3a1 1 0 0 0-.3 1.4l2.1 4.1v.2l.4 1c.2.5.7.8 1.2.7z" ] []
        ]

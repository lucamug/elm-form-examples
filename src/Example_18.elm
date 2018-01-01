module Main exposing (main)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Svg
import Svg.Attributes
import SvgIcons
import Utils
import Validate


exampleVersion : String
exampleVersion =
    "17"


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
    }


initialModel : Model
initialModel =
    { errors = []
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
    }


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


type FormField
    = Email
    | Password



-- UPDATE


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



-- HELPERS


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



-- VIEWS


view : Model -> Html Msg
view model =
    Utils.view model exampleVersion viewForm


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
                    , ( "upperPosition", hasFocus || (not hasFocus && content /= "") )
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
                SvgIcons.svgApple

            "Banana" ->
                SvgIcons.svgBanana

            "Orange" ->
                SvgIcons.svgOrange

            "Pear" ->
                SvgIcons.svgPear

            "Strawberry" ->
                SvgIcons.svgStrawberry

            "Cherry" ->
                SvgIcons.svgCherry

            "Grapes" ->
                SvgIcons.svgGrapes

            "Watermelon" ->
                SvgIcons.svgWatermelon

            "Pineapple" ->
                SvgIcons.svgPineapple

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



-- SVG


svgHide : String -> Html msg
svgHide color =
    Svg.svg [ Svg.Attributes.viewBox "0 0 512 512", Svg.Attributes.height "32", Svg.Attributes.width "32" ]
        [ Svg.path
            [ Svg.Attributes.fill
                color
            , Svg.Attributes.d
                "M506 241l-89-89-14-13-258 258a227 227 0 0 0 272-37l89-89c8-8 8-22 0-30zM256 363a21 21 0 0 1 0-43c35 0 64-29 64-64a21 21 0 0 1 43 0c0 59-48 107-107 107zM95 152L6 241c-8 8-8 22 0 30l89 89 14 13 258-258c-86-49-198-37-272 37zm161 40c-35 0-64 29-64 64a21 21 0 0 1-43 0c0-59 48-107 107-107a21 21 0 0 1 0 43z"
            ]
            []
        ]


svgShow : String -> Html msg
svgShow color =
    Svg.svg [ Svg.Attributes.viewBox "0 0 512 512", Svg.Attributes.height "32", Svg.Attributes.width "32" ]
        [ Svg.path
            [ Svg.Attributes.fill
                color
            , Svg.Attributes.d
                "M256 192a64 64 0 1 0 0 128 64 64 0 0 0 0-128zm250 49l-89-89c-89-89-233-89-322 0L6 241c-8 8-8 22 0 30l89 89a227 227 0 0 0 322 0l89-89c8-8 8-22 0-30zM256 363a107 107 0 1 1 0-214 107 107 0 0 1 0 214z"
            ]
            []
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

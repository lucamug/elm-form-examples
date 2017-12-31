module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Svg
import Svg.Attributes
import Utils
import Validate


exampleVersion : String
exampleVersion =
    "12"


type alias Model =
    { errors : List Error
    , email : String
    , password : String
    , response : Maybe String
    , focus : Maybe FormField
    , showErrors : Bool
    , showPassword : Bool
    }


initialModel : Model
initialModel =
    { errors = []
    , email = ""
    , password = ""
    , response = Nothing
    , focus = Nothing
    , showErrors = False
    , showPassword = False
    }


type alias Error =
    ( FormField, String )


type Msg
    = NoOp
    | SubmitForm
    | SetField FormField String
    | Response (Result Http.Error String)
    | OnFocus FormField
    | OnBlur FormField
    | ToggleShowPasssword


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
                    ( { model | errors = [], response = Nothing }
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
            ( { model | response = Just response }, Cmd.none )

        Response (Err error) ->
            ( { model | response = Just (toString error ++ " - See the Console for more details.") }, Cmd.none )

        OnFocus formField ->
            ( { model | focus = Just formField }, Cmd.none )

        OnBlur formField ->
            ( { model | focus = Nothing }, Cmd.none )

        ToggleShowPasssword ->
            ( { model | showPassword = not model.showPassword }, Cmd.none )



-- HELPERS


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


postRequest : Model -> Http.Request String
postRequest model =
    let
        body =
            Encode.object
                [ ( "email", Encode.string model.email )
                , ( "password", Encode.string model.password )
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
    in
    label
        []
        [ text inputName
        , div [ class "inputFieldContainer" ]
            [ input
                [ if formField == Password && model.showPassword then
                    type_ "text"
                  else
                    type_ inputType
                , classList
                    [ ( "focus", hasFocus ) ]
                , placeholder inputName
                , onInput <| SetField formField
                , onFocus <| OnFocus formField
                , onBlur <| OnBlur formField
                , value <|
                    case formField of
                        Email ->
                            model.email

                        Password ->
                            model.password
                ]
                []
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
    Html.div
        [ class "form-container"
        , onEnter SubmitForm
        ]
        [ node "style" [] [ text "" ]
        , viewInput model Email "text" "Email"
        , viewInput model Password "password" "Password"
        , button [ onClick SubmitForm ] [ text "Submit" ]
        ]


viewFormErrors : Model -> FormField -> List Error -> Html msg
viewFormErrors model field errors =
    if model.showErrors then
        errors
            |> List.filter (\( fieldError, _ ) -> fieldError == field)
            |> List.map (\( _, error ) -> li [] [ text error ])
            |> ul [ class "formErrors" ]
    else
        text ""



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

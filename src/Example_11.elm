module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Utils
import Validate


exampleVersion : String
exampleVersion =
    "11"


type alias Model =
    { errors : List Error
    , email : String
    , password : String
    , response : Maybe String
    , focus : Maybe FormField
    , showErrors : Bool
    }


initialModel : Model
initialModel =
    { errors = []
    , email = ""
    , password = ""
    , response = Nothing
    , focus = Nothing
    , showErrors = False
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
        , input
            [ type_ inputType
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



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

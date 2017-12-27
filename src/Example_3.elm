module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Utils
import Validate


exampleVersion : String
exampleVersion =
    "3"


type alias Model =
    { errors : List Error
    , email : String
    , password : String
    , response : Maybe String
    }


initialModel : Model
initialModel =
    { errors = []
    , email = ""
    , password = ""
    , response = Nothing
    }


type alias Error =
    ( FormField, String )


type Msg
    = NoOp
    | SubmitForm
    | SetEmail String
    | SetPassword String
    | Response (Result Http.Error String)


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
                    ( { model | errors = errors }
                    , Cmd.none
                    )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        Response (Ok response) ->
            ( { model | response = Just response }, Cmd.none )

        Response (Err error) ->
            ( { model | response = Just (toString error ++ " - See the Console for more details.") }, Cmd.none )



--HELPERS


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



-- VIEWS


view : Model -> Html Msg
view model =
    Utils.view model exampleVersion viewForm viewResponse


viewForm : Model -> Html Msg
viewForm model =
    Html.form
        [ onSubmit SubmitForm
        , class "form-container"
        ]
        [ label []
            [ text "Email"
            , viewFormErrors Email model.errors
            , input
                [ type_ "text"
                , placeholder "Email"
                , onInput SetEmail
                , value model.email
                ]
                []
            ]
        , label []
            [ text "Password"
            , viewFormErrors Password model.errors
            , input
                [ type_ "password"
                , placeholder "Password"
                , onInput SetPassword
                , value model.password
                ]
                []
            ]
        , button
            []
            [ text "Submit" ]
        ]


viewForm2 : Model -> Html Msg
viewForm2 model =
    Html.form
        [ onSubmit SubmitForm
        , class "form-container"
        ]
        [ viewFormErrors Email model.errors
        , div []
            [ input
                [ type_ "text"
                , placeholder "Email"
                , onInput SetEmail
                , value model.email
                ]
                []
            ]
        , viewFormErrors Password model.errors
        , div []
            [ input
                [ type_ "password"
                , placeholder "Password"
                , onInput SetPassword
                , value model.password
                ]
                []
            ]
        , button []
            [ text "Submit" ]
        ]


viewResponse : String -> Html msg
viewResponse response =
    div [ class "response-container" ]
        [ h2 [] [ text "Response" ]
        , textarea []
            [ text response ]
        ]


viewFormErrors : FormField -> List Error -> Html msg
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "formErrors" ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

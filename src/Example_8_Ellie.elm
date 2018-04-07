module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Validate


exampleVersion : String
exampleVersion =
    "8"


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
    | SetField FormField String
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
        , url = urlMirrorService
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
    viewNormal model exampleVersion viewForm


viewForm : Model -> Html Msg
viewForm model =
    Html.div
        [ class "form-container"
        ]
        [ label []
            [ text "Email"
            , viewFormErrors Email model.errors
            , input
                [ type_ "text"
                , placeholder "Email"
                , onInput <| SetField Email
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
                , onInput <| SetField Password
                , value model.password
                ]
                []
            ]
        , button
            [ onClick SubmitForm
            , classList
                [ ( "disabled", not <| List.isEmpty model.errors ) ]
            ]
            [ text "Submit" ]
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



-- Utils


exampleComment : Dict.Dict String String
exampleComment =
    Dict.fromList
        [ "1" => "First dumb version of the form. Just an old simple form, not the proper way to build forms in Elm"
        , "2" => "Changed the form to be à la Elm using \"application/x-www-form-urlencoded\" as encoding system"
        , "3" => "Changed the encoding to json"
        , "4" => "Added validation"
        , "5" => "Moved the field updates out of the update function"
        , "6" => "Replaced the <form> element with <div> and adding \"onClick SubmitForm\" to the button"
        , "7" => "Bringing back the onEnter behavior that submit the form pressing Enter when input fields have focus"
        , "8" => "Added validation while typing and disabled Submit button"
        ]


viewFooter : String -> Html msg
viewFooter version =
    div [ class "footer" ]
        [ text "https://github.com/lucamug/elm-form-examples" ]


urlMirrorService : String
urlMirrorService =
    "https://httpbin.org/post"


viewHeader : String -> Html msg
viewHeader version =
    div [ class "header" ]
        [ h1 [] [ text ("Elm Forms - Example " ++ version) ]
        , p [] [ text <| getComment version ]
        ]


viewSimple : String -> Html msg -> Html msg
viewSimple exampleVersion viewForm =
    div []
        [ viewHeader exampleVersion
        , viewForm
        , viewFooter exampleVersion
        ]


viewNormal :
    { a | response : Maybe String }
    -> String
    -> ({ a | response : Maybe String } -> Html msg)
    -> Html msg
viewNormal model exampleVersion viewForm =
    div []
        [ viewHeader exampleVersion
        , viewForm model
        , case model.response of
            Just response ->
                viewResponse response

            Nothing ->
                text ""
        , viewFooter exampleVersion
        ]


viewResponse : String -> Html msg
viewResponse response =
    div [ class "response-container" ]
        [ h2 [] [ text "Response" ]
        , textarea []
            [ text response ]
        ]


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


getComment : String -> String
getComment version =
    Dict.get version exampleComment
        |> Maybe.withDefault ""

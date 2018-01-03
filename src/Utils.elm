module Utils exposing (urlMirrorService, view, viewUtils)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)


exampleComment : Dict.Dict String String
exampleComment =
    Dict.fromList
        [ "index" => "Examples of Form built in elm."
        , "1" => "First version: just an old simple form."
        , "2" => "Changed the form to be à la Elm using \"application/x-www-form-urlencoded\" as encoding system"
        , "3" => "Changed the encoding system to json"
        , "4" => "Added validation"
        , "5" => "Moved the field updates out of the update function"
        , "6" => "Replaced the <form> element with <div> and added \"onClick SubmitForm\" to the button"
        , "7" => "Restored the \"submit-on-enter\" behavior"
        , "8" => "Added validation while typing"
        , "9" => "Created the helper \"viewInput\" that generalized the creation of input fields"
        , "10" => "Adding \"showErrors\" functionality that show error only after the first submit "
        , "11" => "Adding focus detection so that focus is evident also during history playback"
        , "12" => "Adding the icon to hide and show the password"
        , "13" => "Adding a spinner while the app is waiting for an answer"
        , "14" => "Adding \"Floating Label\""
        , "15" => "Adding Checkboxes"
        , "16" => "Encoded Checkboxes values into the Json for sending to the server"
        , "17" => "Adding maximum number of checkable fruits"
        , "18" => "Adding svg fruit icons"
        , "19" => "Adding a date picker"
        ]


viewFooter : String -> Html msg
viewFooter version =
    div [ class "footer" ]
        [ a [ href "https://github.com/lucamug/elm-form-examples" ]
            [ text "[ code ] " ]
        , a [ href "https://medium.com/@l.mugnaini/forms-in-elm-validation-tutorial-and-examples-2339830055da" ] [ text " [ article ]" ]
        ]


urlMirrorService : String
urlMirrorService =
    "http://httpbin.org/post"


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


viewUtils :
    { a | response : Maybe String }
    -> String
    -> ({ a | response : Maybe String } -> Html msg)
    -> Html msg
viewUtils model exampleVersion viewForm =
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


view :
    { a | response : Maybe String }
    -> String
    -> ({ a | response : Maybe String } -> Html msg)
    -> Html msg
view =
    viewUtils


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

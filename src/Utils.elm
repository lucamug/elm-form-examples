module Utils exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)


exampleComment : Dict.Dict String String
exampleComment =
    Dict.fromList
        [ "1" => "First dumb version of the form. Just an old simple form, not the proper way to build forms in Elm"
        , "2" => "Changed the form to be à la Elm using \"application/x-www-form-urlencoded\" as encoding system"
        , "3" => "Changed the encoding to json"
        , "4" => "Added validation"
        , "5" => "Moved the field updates out of the update function"
        , "6" => "Replaced the <form> element with <div> and adding \"onClick SubmitForm\" to the button"
        , "7" => "Bringing back the onEnter behaviour that submit the form pressing Enter when input fields have focus"
        , "8" => "Added validation while typing and disabled Submit button"
        ]


viewFooter : String -> Html msg
viewFooter version =
    div [ class "footer" ]
        [ text "https://github.com/lucamug/elm-form-examples" ]


urlMirrorService : String
urlMirrorService =
    "http://httpbin.org/post"


viewHeader : String -> Html msg
viewHeader version =
    div [ class "header" ]
        [ h1 [] [ text ("Elm Forms - Example " ++ version) ]
        , p [] [ text <| getComment version ]
        ]


view :
    { b | response : Maybe a }
    -> String
    -> ({ b | response : Maybe a } -> Html msg)
    -> (a -> Html msg)
    -> Html msg
view model exampleVersion viewForm viewResponse =
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


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


getComment : String -> String
getComment version =
    Dict.get version exampleComment
        |> Maybe.withDefault ""



-- "https://www.w3schools.com/action_page.php"

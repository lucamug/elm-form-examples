module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Utils


exampleVersion : String
exampleVersion =
    "1"



-- VIEWS


view : Html msg
view =
    Utils.viewSimple exampleVersion viewForm


viewForm : Html msg
viewForm =
    Html.form
        [ action Utils.urlMirrorService
        , method "post"
        , class "form-container"
        ]
        [ label []
            [ text "Email"
            , input
                [ type_ "text"
                , placeholder "Email"
                , name "email"
                ]
                []
            ]
        , label []
            [ text "Password"
            , input
                [ type_ "password"
                , placeholder "Password"
                , name "password"
                ]
                []
            ]
        , button
            []
            [ text "Submit" ]
        ]


main : Html msg
main =
    view

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Utils


exampleVersion : String
exampleVersion =
    "1"


view : Html msg
view =
    div []
        [ Utils.viewHeader exampleVersion
        , viewForm
        , Utils.viewFooter exampleVersion
        ]


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


viewForm2 : Html msg
viewForm2 =
    Html.form
        [ action Utils.urlMirrorService
        , method "post"
        ]
        [ div []
            [ input
                [ type_ "text"
                , placeholder "Email"
                , name "email"
                ]
                []
            ]
        , div []
            [ input
                [ type_ "password"
                , placeholder "Password"
                , name "password"
                ]
                []
            ]
        , button []
            [ text "Submit" ]
        ]


main : Html msg
main =
    view

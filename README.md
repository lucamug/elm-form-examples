# Elm Scroll Resize Events

## [Demo with Time-traveling Debugger](https://lucamug.github.io/elm-form-examples/)

# How it works

Check out [the post]()!

# Getting started

If you don't already have `elm` and `elm-live`:

> npm install -g elm elm-live

Then, to build everything:

> elm-live --output=Example_1.js src/Example_1.elm --open --debug
> elm-live --output=Example_2.js src/Example_2.elm --open --debug
> elm-live --output=Example_3.js src/Example_3.elm --open --debug
> elm-live --output=Example_4.js src/Example_4.elm --open --debug
> elm-live --output=Example_5.js src/Example_5.elm --open --debug
> elm-live --output=Example_6.js src/Example_6.elm --open --debug
> elm-live --output=Example_7.js src/Example_7.elm --open --debug
> elm-live --output=Example_8.js src/Example_8.elm --open --debug

(Leave off the `--debug` if you don't want the time-traveling debugger.)

## About forms

Forms are send, by default, in "application/x-www-form-urlencoded"(https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/POST) format, where the values are encoded in key-value tuples separated by '&', with a '=' between the key and the value. Non-alphanumeric characters are percent-encoded(https://developer.mozilla.org/en-US/docs/Glossary/percent-encoding).

## Encoding in Elm

https://github.com/elm-lang/url/blob/master/examples/Example.elm

## Notes

- Loosing type safety:

    http://package.elm-lang.org/packages/etaque/elm-form/latest
    http://etaque.github.io/elm-form/example/

Do I needed

onWithOptions
    "submit"
    { preventDefault = True, stopPropagation = False }
    (Json.Decode.succeed SubmitPost)

## Resources

https://github.com/evancz/elm-http/issues/22
https://github.com/lukewestby/elm-http-builder
http://package.elm-lang.org/packages/krisajenkins/elm-exts/27.4.0/Exts-Http
https://github.com/rtfeldman/elm-spa-example/blob/master/src/Page/Login.elm
https://github.com/rtfeldman/elm-spa-example/blob/master/src/Request/User.elm
https://stackoverflow.com/questions/36387409/how-to-submit-a-form-in-elm


## Forms in Elm

Sometime new comers to Elm complain about the complexity and the large amount of boilerplate needed to implement forms in Elm.

Forms are the main interaction point between users and applications and should be simple to build.

Most of this post is derived by the the SPA built by rtfeldman (https://github.com/rtfeldman/elm-spa-example/blob/master/src/Page/Login.elm#L62-L79)

## Example 1 - A "fake" Elm form

Code: https://github.com/lucamug/elm-form-examples/blob/master/src/Example_1.elm
Demo: https://lucamug.github.io/elm-form-examples/Example_1.html

Let's start with a simple (and useless) exercise. Build an old fashion form, the way we were doing in ancient time, but using Elm. This is what it could be the result:

viewForm : Html msg
viewForm =
    Html.form
        [ action "http://httpbin.org/post"
        , method "post"
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

Nothing special here, exactly what an Html form would look like. This form send a POST request in "application/x-www-form-urlencoded"(https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/POST) format, where the values are percent-encoded(https://developer.mozilla.org/en-US/docs/Glossary/percent-encoding) and concatenated in key-value tuples separated by '&', with a '=' between the key and the value.

On submitting the form, this text is sent in the body request:

email=foo&password=bar

I am using http://httpbin.org/post as a server service that receive the received data. Unfortunately this service doesn't return the content of the body as it is, but it parses it as Json data.

In the response, among other data, you can find:

{
    "data": "",
    "form": {
        "email": "foo",
        "password": "bar"
    },
    "headers": {
        "Content-Type": "application/x-www-form-urlencoded",
        ...
    },
    ...
}

Of course the browse will move out from our Elm app and this is not exactly what a nice and fast Single Page Application should do.

So let's Elm-ize it!


## Example 2 - Let's change to real Elm form

Code: https://github.com/lucamug/elm-form-examples/blob/master/src/Example_2.elm
Demo: https://lucamug.github.io/elm-form-examples/Example_2.html

There are several things to add/modify here:

1. Create a model structure where to store the data of the form (password and email in our example)
2. Create the necessary messages to handle the flow of data:

type Msg
    = NoOp
    | SubmitForm
    | SetEmail String
    | SetPassword String
    | Response (Result Http.Error String)

3. Create a data type for the Form field. This would make the compiler helping us during the development because it will complain everytime we forget to handle one field of the form

type FormField
    = Email
    | Password

4. Create the update function that will handle the messages. The SubmitForm message will create an Http.send(http://package.elm-lang.org/packages/elm-lang/http/latest/Http#send) command (as data) and send it to the Elm runtime to handle it.

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        SubmitForm ->
            ( { model | response = Nothing }
            , Http.send Response (postRequest model)
            )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        Response (Ok response) ->
            ( { model | response = Just response }, Cmd.none )

        Response (Err error) ->
            ( { model | response = Just (toString error ++ " - See the Console for more details.") }, Cmd.none )

5. Create the function postRequest that will prepare the request that need to be sent. We want to simulate the default behavior of the form, sending the data as "x-www-form-urlencoded" so I manually joined all data together and assigned it to "body". Then I used  Http.request(http://package.elm-lang.org/packages/elm-lang/http/latest/Http#request) combined with Http.expectString(http://package.elm-lang.org/packages/elm-lang/http/latest/Http#expectString) because I am only interesting in receiving the result as string, without parsing.

postRequest : Model -> Http.Request String
postRequest model =
    let
        body =
            [ ( "email", model.email )
            , ( "password", model.password )
            ]
                |> List.map
                    (\( name, value ) -> Http.encodeUri name ++ "=" ++ Http.encodeUri value)
                |> String.join "&"
                |> Http.stringBody "application/x-www-form-urlencoded"
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

6. Update the view removing unnecessary stuff and adding the needed one:

    - Remove "action" and "method" parameters from the <form> element
    - Add "onSubmit" event so that we take control when the form is submitted
    - Remove the name attribute of the input fields
    - Add "onInput" in each input element so that we can update the model when the conent is changing
    - Add the "value" parameter. This is not compulsory but give the nice effect or updating the input fields while replaying the history Debugger

    So the new view is now:

viewForm : Model -> Html Msg
viewForm model =
    Html.form
        [ onSubmit SubmitForm
        , class "form-container"
        ]
        [ label []
            [ text "Email"
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

    Done. The list was not that short for converting a form into an Elm form but from now on everything will be simpler and the benefits huge.

## Example 3 - Moving to Json

Code: https://github.com/lucamug/elm-form-examples/blob/master/src/Example_3.elm
Demo: https://lucamug.github.io/elm-form-examples/Example_3.html

Unless the server is forcing you to stick to "x-www-form-urlencoded", is time to move to Json. Let's change the body part of the post request. from

body =
    [ ( "email", model.email )
    , ( "password", model.password )
    ]
        |> List.map
            (\( name, value ) -> Http.encodeUri name ++ "=" ++ Http.encodeUri value)
        |> String.join "&"
        |> Http.stringBody "application/x-www-form-urlencoded"

to

body =
    Encode.object
        [ ( "email", Encode.string model.email )
        , ( "password", Encode.string model.password )
        ]
        |> Http.jsonBody

So instead of manually encoding stuff, we used Encode.object and Encode.string. Moreover we replace Http.stringBody(http://package.elm-lang.org/packages/elm-lang/http/latest/Http#stringBody) with Http.jsonBody(http://package.elm-lang.org/packages/elm-lang/http/latest/Http#jsonBody).

Now the response chaged from

{
    "data": "",
    "form": {
        "email": "foo",
        "password": "bar"
    },
    "headers": {
        "Content-Type": "application/x-www-form-urlencoded",
        ...
    },
    ...
}

to

{
    "data": "{\"email\":\"foo\",\"password\":\"bar\"}",
    "form": {},
    "headers": {
        "Content-Type": "application/json",
        ...
    },
    ...
}

## Example 4 - Adding Validation

Code: https://github.com/lucamug/elm-form-examples/blob/master/src/Example_4.elm
Demo: https://lucamug.github.io/elm-form-examples/Example_4.html

Let's see what it takes to add some client side validation.

1. Add a list of Error in the model and a new type alias:

type alias Error =
    ( FormField, String )

2. Create a validate function using rtfeldman/elm-validate(http://package.elm-lang.org/packages/rtfeldman/elm-validate/latest)

validate : Model -> List Error
validate =
    Validate.all
        [ .email >> Validate.ifBlank ( Email, "Email can't be blank." )
        , .password >> Validate.ifBlank ( Password, "Password can't be blank." )
        ]

3. Create a function that filter errors field by field so is possible to display them next to each field in the form:

viewFormErrors : FormField -> List Error -> Html msg
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "formErrors" ]

and add these section inside the form with

viewFormErrors Email model.errors

and

viewFormErrors Password model.errors

4. Modify the SubmitForm handling in the update function form

SubmitForm ->
    ( { model | response = Nothing }
    , Http.send Response (postRequest model)
    )

to

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

So that if there are errors, the forms is not submitted (Cmd.none).

## Example 5 - Moved the field updates out of the update function

Code: https://github.com/lucamug/elm-form-examples/blob/master/src/Example_5.elm
Demo: https://lucamug.github.io/elm-form-examples/Example_5.html

To reduce the size of the update function we can apply these modifications

1. Consolidate all messages related to update form fields into one. From:

type Msg
    = NoOp
    | SubmitForm
    | SetEmail String
    | SetPassword String
    | Response (Result Http.Error String)

to

type Msg
    = NoOp
    | SubmitForm
    | SetField FormField String
    | Response (Result Http.Error String)


2. Create an helper function:

setField : Model -> FormField -> String -> Model
setField model field value =
    case field of
        Email ->
            { model | email = value }

        Password ->
            { model | password = value }

3.In the update function replace

SetEmail email ->
    ( { model | email = email }, Cmd.none )

SetPassword password ->
    ( { model | password = password }, Cmd.none )

with

SetField field value ->
    ( setField model field value, Cmd.none )

4. In the view, replace

onInput SetEmail
onInput SetPassword

with

onInput <| SetField Email
onInput <| SetField Password

There is a way to simplify even further so to reduce the number of code but this would necessarily loose type safety. That means that the field name will be stored and passed as string so the compiler will not be able to cover you in case you are missing something os mistyping something. To move forward in this direction have a look at the excellent package etaque/elm-form (http://package.elm-lang.org/packages/etaque/elm-form/latest), you can find here an excellent example with client side validation: http://etaque.github.io/elm-form/example/


## Example 6 - Replaced the <form> element with <div> and adding \"onClick SubmitForm\" to the button

Code: https://github.com/lucamug/elm-form-examples/blob/master/src/Example_6.elm
Demo: https://lucamug.github.io/elm-form-examples/Example_6.html

What is about getting rid of the <form> element now? We can.

In the view:

1. Let's replace Html.form with Html.div and remove the onSubmit SubmitForm
2. Let's add "onClick SubmitForm" to de button

Let's also add

classList
    [ ( "disabled", not <| List.isEmpty model.errors ) ]

to the button, so that when there are errors, the buttons will appear disabled.

So, now we are completely disconnected from the form element. One issue that we are facing is the submission by pressing Enter is not working anymore but we can bring it back:

Consider that there are library, like mdgriffith/style-element (http://package.elm-lang.org/packages/mdgriffith/style-elements/latest), that don't even expose a form element. In this case, this is the only "clean" way of creating forms, unless you play with nodes wiht something like

node "form" <| el YourStyle [ onClick YourMsg ] []

## Example 7 - Bringing back the onEnter behavior that submit the form pressing Enter when input fields have focus

Code: https://github.com/lucamug/elm-form-examples/blob/master/src/Example_7.elm
Demo: https://lucamug.github.io/elm-form-examples/Example_7.html

1. Create a function to detect the press of Enter button:

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

2. Add "onEnter SubmitForm" to the div that wrap the form

## Example 8 - Added validation while typing and disabled Submit button

Code: https://github.com/lucamug/elm-form-examples/blob/master/src/Example_8.elm
Demo: https://lucamug.github.io/elm-form-examples/Example_8.html

The last small thing that we can add is the validation running while changin the text in the form.

This is a simple implementation that is not optimal because, for example, the validation should not start before the first submit. But just to keep the thing simple:

1. Add the HELPERS

setErrors : Model -> Model
setErrors model =
    case validate model of
        [] ->
            { model | errors = [] }

        errors ->
            { model | errors = errors }

2. Change how update SetField message, from

SetField field value ->
    ( setField field value model, Cmd.none )

to

SetField field value ->
    ( model
        |> setField field value
        |> setErrors
    , Cmd.none
    )

This is an Ellie of the last Example 8: https://ellie-app.com/cKqgwZKf3a1/1

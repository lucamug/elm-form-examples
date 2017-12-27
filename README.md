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

## Resources

https://github.com/evancz/elm-http/issues/22
https://github.com/lukewestby/elm-http-builder
http://package.elm-lang.org/packages/krisajenkins/elm-exts/27.4.0/Exts-Http
https://github.com/rtfeldman/elm-spa-example/blob/master/src/Page/Login.elm
https://github.com/rtfeldman/elm-spa-example/blob/master/src/Request/User.elm
https://stackoverflow.com/questions/36387409/how-to-submit-a-form-in-elm

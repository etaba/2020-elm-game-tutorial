module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--     https://guide.elm-lang.org/architecture/buttons.html
--
--
-- If Elm's syntax looks weird to you, read this first:
--     https://guide.elm-lang.org/core_language.html
-- and here's a reference that might be handy:
--     https://elm-lang.org/docs/syntax
--

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyPress, onKeyUp)
import Browser.Navigation exposing (Key)
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Debug exposing (log)
import Html exposing (Html, del, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { rotationSpeed : Float -- how fast should the square spin?
    , rotation : Float -- what's the current rotation of the square?
    , spinningPaused : Bool -- is the square's spinning paused?
    , enterKeyDown : Bool -- is the enter key held down currently?

    -- Tip: you will be tempted to add a few more "fooKeyDown" states, but that's going to be a lot
    -- of boilerplate. Instead, try saving a list of keys that are down, and making a isKeyDown
    -- helper function that takes in that list + the key you're interested in, then use that
    -- instead.
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { rotationSpeed = 0.25
      , rotation = 0
      , spinningPaused = False
      , enterKeyDown = False
      }
    , Cmd.none
    )


type Msg
    = Frame Float -- float = dt, aka delta time: the amount of time elapsed since the last frame
    | KeyPressed SupportedKey
    | KeyDowned SupportedKey
    | KeyUpped SupportedKey


type SupportedKey
    = SpaceKey
    | EnterKey
    | UnknownKey


{-| Whether to log debug information about keyboard key events.

Change this to True if you need to debug your input handling.

-}
shouldLogKeyboardEvents : Bool
shouldLogKeyboardEvents =
    False


{-| Wrapper around Debug.log that only logs if the provided boolean is True.
-}
logIf : Bool -> String -> a -> a
logIf condition msg item =
    if condition then
        log msg item

    else
        item


{-| Contains the main game update logic.

updateFrame model dt = This function is called roughly once per monitor refresh (so 60 times a
second if your monitor runs at 60hz); if you trace back how this is called, you'll see that it's
hooked into the Elm equivalent of
<https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame>

The exact time between runs varies depending on CPU load, which is what the dt param is for: it
tracks the amount of time elapsed since this function was called last, so any time you move anything
in the game world, the movement needs to be multiplied by dt. That way you won't have e.g. a
character run slower on a slow computer than a fast computer.

-}
updateFrame : Model -> Float -> Model
updateFrame model dt =
    let
        adjustedRotationSpeed =
            if model.spinningPaused then
                0

            else
                model.rotationSpeed
    in
    { model | rotation = model.rotation + adjustedRotationSpeed * dt }


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    \msg model ->
        let
            updatedModel =
                case msg of
                    Frame deltaTime ->
                        updateFrame model deltaTime

                    KeyPressed key ->
                        case logIf shouldLogKeyboardEvents "update: Key was pressed" key of
                            SpaceKey ->
                                { model | spinningPaused = not model.spinningPaused }

                            _ ->
                                model

                    KeyDowned key ->
                        case logIf shouldLogKeyboardEvents "update: Key was held down" key of
                            EnterKey ->
                                { model | enterKeyDown = True }

                            _ ->
                                model

                    KeyUpped key ->
                        case logIf shouldLogKeyboardEvents "update: Key was released" key of
                            EnterKey ->
                                { model | enterKeyDown = False }

                            _ ->
                                model
        in
        ( updatedModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onKeyPress (Decode.map KeyPressed keyDecoder)
        , onKeyDown (Decode.map KeyDowned keyDecoder)
        , onKeyUp (Decode.map KeyUpped keyDecoder)
        ]


keyDecoder : Decode.Decoder SupportedKey
keyDecoder =
    Decode.map parseKey (Decode.field "key" Decode.string)


parseKey : String -> SupportedKey
parseKey rawKey =
    let
        parsedKey =
            case rawKey of
                " " ->
                    SpaceKey

                "Enter" ->
                    EnterKey

                _ ->
                    UnknownKey
    in
    -- `Debug.log` takes 2 args, a string description and the thing you want to log; then it
    -- console.logs the thing + returns the thing. So you can use it to wrap anything and see what
    -- its value is on the console. Just remember that it always needs two args!
    -- https://package.elm-lang.org/packages/elm/core/latest/Debug#log
    logIf shouldLogKeyboardEvents ("parseKey: from '" ++ rawKey ++ "' we parsed a key of") parsedKey


width : number
width =
    800


height : number
height =
    600


centerX : Float
centerX =
    width / 2


centerY : Float
centerY =
    height / 2


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            [ clearScreen
            , render model
            ]
        ]


clearScreen : Canvas.Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render : Model -> Canvas.Renderable
render model =
    let
        rectSize =
            width / 3

        x =
            -(rectSize / 2)

        y =
            -(rectSize / 2)

        rotation =
            degrees model.rotation

        hue =
            toFloat (model.rotation / 4 |> floor |> modBy 100) / 100
    in
    -- Read the elm-canvas docs to understand how to use `shapes`:
    -- https://package.elm-lang.org/packages/joakin/elm-canvas/latest/
    shapes
        [ transform
            [ translate centerX centerY
            , rotate rotation
            ]
        , fill (Color.hsl hue 0.3 0.7)
        ]
        [ rect ( x, y ) rectSize rectSize ]

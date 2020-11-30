module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyPress, onKeyUp)
import Browser.Navigation exposing (Key)
import Canvas exposing (circle, rect, path, lineTo, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Debug exposing (log)
import Html exposing (Html, del, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Dict exposing (Dict)
--import Time exposing (Posix)
import Random exposing (Generator)



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { rotationSpeed : Float
    , vx : Float
    , vy : Float
    , spinningPaused : Bool
    , upKeyDown : Bool
    , rightKeyDown : Bool
    , leftKeyDown : Bool
    , spaceKeyDown : Bool
    , shipCoordinate : Coordinate
    , bullets : List Coordinate
    , asteroids : List Asteroid
    , numAsteroids : Int
    , randomInt : Int
    , gameOver : Bool
    }

type alias Coordinate =
    { x : Float
    , y : Float
    , rotation : Float
    }

type alias Asteroid =
    { coordinate : Coordinate
    , radius : Float
    , speed : Float
  }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { rotationSpeed = 0.45
      , vx = 0
      , vy = 0
      , spinningPaused = False
      , upKeyDown = False
      , rightKeyDown = False
      , leftKeyDown = False
      , spaceKeyDown = False
      , shipCoordinate = { x = width/2, y = height/2, rotation = 0}
      , bullets = []
      , asteroids = []
      , numAsteroids = 100
      , randomInt = 0
      , gameOver = False }
    , Cmd.none
    )


type Msg
    = Frame Float -- float = dt, aka delta time: the amount of time elapsed since the last frame
    | KeyPressed SupportedKey
    | KeyDowned SupportedKey
    | KeyUpped SupportedKey
    | GotRandom Int


type SupportedKey
    = SpaceKey
    | UpKey
    | LeftKey
    | RightKey
    | UnknownKey


randomFloat : Float -> Float -> Model -> Float
randomFloat begin end model =
  ((end - begin) * (toFloat model.randomInt)/10 + begin)

newAsteroids : Model -> List Asteroid
newAsteroids model = 
  if List.length model.asteroids < model.numAsteroids then
    let
      newAsteroid = createAsteroid model
      asteroidTouching = List.length (List.filter (\a -> asteroidCollision newAsteroid a.coordinate a.radius) model.asteroids) > 0
    in
      if asteroidTouching then
        []
      else
        List.singleton (newAsteroid)
  else
    []

createAsteroid : Model -> Asteroid
createAsteroid model =
  let
    speed = randomFloat 0.1 0.5 model
    radius = randomFloat 10 100 model

    --is the asteroid starting on the top, right, bottom, or left of screen
    startSide = (floor (randomFloat 0 4 model))
  in
    case startSide of
      0 ->
        --top
        {
          coordinate = { 
            x = (randomFloat (-1 * radius) (width + radius) model),
            y = (-1 * radius),
            rotation = (randomFloat 0 180 model)
          },
          radius = radius,
          speed = speed
        }
      1 ->
        --right
        {
          coordinate = { 
            x = width + radius,
            y = randomFloat (-1 * radius) (height + radius) model,
            rotation = randomFloat 90 270 model
          },
          radius = radius,
          speed = speed
        }
      2 ->
        --bottom
        {
          coordinate = { 
            --x = randomFloat (-1 * radius) (width + radius) model,
            x = width/2,
            y = height + radius,
            rotation = (randomFloat -180 0 model)
          },
          radius = radius,
          speed = speed
        }
      _ ->
        --left
        {
          coordinate = { 
          x = -1 * radius,
          y = randomFloat (-1 * radius) (height + radius) model,
          rotation = randomFloat -90 90 model
          },
          radius = radius,
          speed = speed
        }

asteroidCollision : Asteroid -> Coordinate -> Float -> Bool
asteroidCollision asteroid coord margin =
  let
    dist = sqrt ((asteroid.coordinate.x - coord.x)^2 + (asteroid.coordinate.y - coord.y)^2)
  in
    dist <= (asteroid.radius + margin)

outOfBounds : Coordinate -> Float -> Bool
outOfBounds coord margin =
  coord.x > (margin + width) || 
  coord.x < (-1 * margin) ||
  coord.y > (height + margin) || 
  coord.y < (-1 * margin)

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
              if model.rightKeyDown then
                model.rotationSpeed
              else if model.leftKeyDown then
                model.rotationSpeed * -1
              else
                0

        adjustedVx0 = 
          if model.upKeyDown then
            if cos(degrees model.shipCoordinate.rotation) >= 0 then
              (max 0 model.vx) + cos(degrees model.shipCoordinate.rotation) * thrust * dt
            else
              (min 0 model.vx) + cos(degrees model.shipCoordinate.rotation) * thrust * dt
          else
            0.95 * model.vx

        adjustedVx = 
          if sqrt (adjustedVx0 * adjustedVx0 + adjustedVy0 * adjustedVy0) > maxShipSpeed then
            maxShipSpeed * cos(degrees model.shipCoordinate.rotation)
          else
            adjustedVx0

        adjustedVy0 =
          if model.upKeyDown then
            if sin(degrees model.shipCoordinate.rotation) >= 0 then
              (max 0 model.vy) + sin(degrees model.shipCoordinate.rotation) * thrust * dt
            else
              (min 0 model.vy) + sin(degrees model.shipCoordinate.rotation) * thrust * dt
          else
            0.95 * model.vy

        adjustedVy = 
          if sqrt (adjustedVx0 * adjustedVx0 + adjustedVy0 * adjustedVy0) > maxShipSpeed then
            maxShipSpeed * sin(degrees model.shipCoordinate.rotation)
          else
            adjustedVy0

        adjustedX =
          if (model.shipCoordinate.x + adjustedVx * dt) > width then
            0
          else if (model.shipCoordinate.x + adjustedVx * dt) < 0 then
            height
          else
            model.shipCoordinate.x + adjustedVx * dt

        adjustedY = 
          if (model.shipCoordinate.y + adjustedVy * dt) > height then
            model.shipCoordinate.y + adjustedVy * dt - height
          else if (model.shipCoordinate.y + adjustedVy * dt) < 0 then
            model.shipCoordinate.y + adjustedVy * dt + height
          else
            model.shipCoordinate.y + adjustedVy * dt

        adjustedRotation = 
          model.shipCoordinate.rotation + adjustedRotationSpeed * dt

        asteroidDestroyed : Asteroid -> Bool
        asteroidDestroyed asteroid =
          (List.length (List.filter (\b -> asteroidCollision asteroid b 0) model.bullets)) > 0

        moveCoordinate : Coordinate -> Float -> Coordinate
        moveCoordinate coord speed =
          let
            adjustedCoordX = coord.x + cos(degrees coord.rotation) * speed *dt
            adjustedCoordY = coord.y + sin(degrees coord.rotation) * speed *dt
          in
            { coord |
              x = adjustedCoordX,
              y = adjustedCoordY }

        moveBullet : Coordinate -> Maybe Coordinate
        moveBullet coord =
          let
            adjustedCoord = moveCoordinate coord bulletSpeed
            newPosition =
              --out of bounds?
              if outOfBounds adjustedCoord 0 then
                Nothing
              else
                Just adjustedCoord
          in
            case newPosition of
              Just value ->
                newPosition
              Nothing ->
                Nothing

        adjustRotation : Asteroid -> Float
        adjustRotation asteroid =
          let
            otherAsteroids = List.filter (\a -> asteroid.radius /= a.radius || asteroid.coordinate /= a.coordinate) model.asteroids
            asteroidTouching = List.head (List.filter (\a -> asteroidCollision asteroid a.coordinate a.radius) otherAsteroids)
          in
            case asteroidTouching of
              Just otherAsteroid ->
                otherAsteroid.coordinate.rotation
              Nothing ->
                asteroid.coordinate.rotation

        moveAsteroid : Asteroid -> Maybe Asteroid
        moveAsteroid asteroid =
          let
            asteroidCoordinate = { x = asteroid.coordinate.x, y = asteroid.coordinate.y, rotation = (adjustRotation asteroid) }
            adjustedCoord = moveCoordinate asteroidCoordinate asteroid.speed
            diameter = 2 * asteroid.radius
            newPosition =
              if outOfBounds adjustedCoord diameter then
                --out of bounds?
                Nothing
              else if asteroidDestroyed { asteroid | coordinate = adjustedCoord} then
                --bullet shot asteroid?
                Nothing
              else
                Just adjustedCoord
          in
            case newPosition of
              Just value ->
                Just { asteroid | coordinate = value }
              Nothing ->
                Nothing

        bullets = List.filterMap moveBullet model.bullets

        asteroids = List.append (List.filterMap moveAsteroid model.asteroids) (newAsteroids model)

        adjustedShipCoordinate = {x = adjustedX, y = adjustedY, rotation = adjustedRotation}

        --gameOver = List.length (List.filter (\a -> (asteroidCollision a adjustedShipCoordinate shipLen)) model.asteroids) > 0
        gameOver = False
    in
    if gameOver then
      model
    else
      { model | vx = adjustedVx, 
                vy = adjustedVy,
                shipCoordinate = adjustedShipCoordinate,
                bullets = bullets,
                asteroids = asteroids,
                gameOver = gameOver
                 }


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    \msg model ->
        let
            updatedModel =
                case msg of
                    Frame deltaTime ->
                        updateFrame model deltaTime

                    KeyPressed key ->
                        case key of
                            SpaceKey ->
                                { model | bullets = (model.shipCoordinate :: model.bullets) }
                            _ ->
                                model

                    KeyDowned key ->
                        case key of
                            UpKey ->
                                { model | upKeyDown = True }
                            RightKey ->
                                { model | rightKeyDown = True }
                            LeftKey ->
                                { model | leftKeyDown = True }
                            SpaceKey ->
                                { model | spaceKeyDown = True }
                            _ ->
                                model

                    KeyUpped key ->
                        case key of
                            UpKey ->
                                { model | upKeyDown = False }

                            RightKey ->
                                { model | rightKeyDown = False }

                            LeftKey ->
                                { model | leftKeyDown = False }

                            SpaceKey ->
                                { model | spaceKeyDown = False }

                            _ ->
                                model

                    GotRandom int ->
                      { model | randomInt = int }
            updatedCmd =
              case msg of
                  GotRandom _ ->
                    Cmd.none
                  _ ->
                    Random.generate GotRandom (Random.int 1 10)
        in
        ( updatedModel, updatedCmd )


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

                "ArrowLeft" ->
                    LeftKey

                "ArrowUp" ->
                    UpKey

                "ArrowRight" ->
                    RightKey

                _ ->
                    UnknownKey
    in
    parsedKey

--constants
width : number
width =
    800

height : number
height =
    500

shipLen : Float
shipLen = 20.0

bulletSpeed : Float
bulletSpeed = 0.5

thrust : Float
thrust = 0.001

maxShipSpeed : Float
maxShipSpeed = 0.4

view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "" "" ]
            ([ clearScreen
            , renderShip model
            ] ++ List.map renderBullet model.bullets ++ List.map renderAsteroid model.asteroids)
        ]


clearScreen : Canvas.Renderable
clearScreen =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]


renderShip : Model -> Canvas.Renderable
renderShip model =
    let
        rotation =
            degrees model.shipCoordinate.rotation

        hue =
            toFloat (model.shipCoordinate.rotation / 4 |> floor |> modBy 100) / 100
    in
    shapes
        [ transform
            [ translate model.shipCoordinate.x model.shipCoordinate.y
            , rotate rotation
            ]
        , fill (Color.hsl hue 0.3 0.7)
        ]
        [ path ( 0, 0 )   [ lineTo ( shipLen, 0), lineTo ( -shipLen, shipLen), lineTo ( -shipLen, -shipLen), lineTo ( shipLen, 0)]]

renderBullet : Coordinate -> Canvas.Renderable
renderBullet coord =
  let
      rotation =
          degrees coord.rotation

      hue =
          toFloat (coord.rotation / 4 |> floor |> modBy 100) / 100
  in
  shapes
      [ transform
          [ translate coord.x coord.y
          , rotate rotation
          ]
      , fill (Color.hsl hue 0.3 0.7)
      ]
      [ circle ( 0, 0 ) 3 ]

renderAsteroid : Asteroid -> Canvas.Renderable
renderAsteroid asteroid =
  let
      rotation =
          degrees asteroid.coordinate.rotation
  in
  shapes
      [ transform
          [ translate asteroid.coordinate.x asteroid.coordinate.y
          , rotate asteroid.coordinate.rotation
          ]
      , fill (Color.gray)
      ]
      [ circle ( 0, 0 ) asteroid.radius ]
module Main exposing (..)

import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time

-- MAIN
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL
type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0)
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform Tick Time.now
      ]
  )

-- UPDATE
type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW

boxSize = 400
xCoord  = 200
yCoord  = 200
radius  = 150

hourMarkRadius = radius - (radius * 0.1)
hourHandRadius = radius - (radius * 0.5)
minHandRadius  = radius - (radius * 0.3)
secHandRadius  = radius - (radius * 0.2)

view : Model -> Html Msg
view model =
    let
        hour   = toFloat (Time.toHour   model.zone model.time)
        minute = toFloat (Time.toMinute model.zone model.time)
        second = toFloat (Time.toSecond model.zone model.time)
    in
        svg [ viewBox "0 0 400 400"
            , width  (String.fromInt boxSize)
            , height (String.fromInt boxSize)
            ]
        [ background
        , viewHand 4 hourHandRadius (hour/12)   "black"
        , viewHand 2 minHandRadius  (minute/60) "grey"
        , viewHand 1 secHandRadius  (second/60) "white"
        , hourMark hourMarkRadius 1
        , hourMark hourMarkRadius 2
        , hourMark hourMarkRadius 3
        , hourMark hourMarkRadius 4
        , hourMark hourMarkRadius 5
        , hourMark hourMarkRadius 6
        , hourMark hourMarkRadius 7
        , hourMark hourMarkRadius 8
        , hourMark hourMarkRadius 9
        , hourMark hourMarkRadius 10
        , hourMark hourMarkRadius 11
        , hourMark hourMarkRadius 12
        ]

background : Svg msg
background =
    circle [ cx (String.fromInt xCoord)
           , cy (String.fromInt yCoord)
           , r (String.fromInt radius)
           , fill "rgb(63, 143, 233)"
           , stroke "black"
           , strokeWidth "2"
           ] []

viewHand : Int -> Float -> Float -> String -> Svg msg
viewHand width length turns colour =
    let
        x = coordX length turns
        y = coordY length turns
    in
        line
        [ x1 (String.fromInt xCoord)
        , y1 (String.fromInt yCoord)
        , x2 (String.fromFloat x)
        , y2 (String.fromFloat y)
        , stroke colour
        , strokeWidth (String.fromInt width)
        , strokeLinecap "round"
        ] []

hourMark : Float -> Int -> Svg msg
hourMark length hour =
    let
        turns = (toFloat hour)/12
        x = coordX length turns
        y = coordY length turns
    in
    circle [ cx (String.fromFloat x)
           , cy (String.fromFloat y)
           , r "3"
           , fill "black"
           ] []

coordX : Float -> Float -> Float
coordX length turns =
    let
        t = 2 * pi * (turns - 0.25)
    in
        200 + length * cos t

coordY : Float -> Float -> Float
coordY length turns =
    let
        t = 2 * pi * (turns - 0.25)
    in
        200 + length * sin t

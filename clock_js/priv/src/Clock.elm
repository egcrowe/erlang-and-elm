module Clock exposing (clock)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)

boxSize = 400
xCoord  = 200
yCoord  = 200
radius  = 150

markRadius     = radius - (radius * 0.1)
hourHandRadius = radius - (radius * 0.5)
minHandRadius  = radius - (radius * 0.3)
secHandRadius  = radius - (radius * 0.2)

clock : Int -> Int -> Int -> Html msg
clock hour minute second =
    let
        handValues = hands hour minute second
    in
    svg [ viewBox "0 0 400 400"
        , width "400"
        , height "400"
        ]
    (List.concat [ [ background ], handValues, hourMarks, minMarks ])

background : Svg msg
background =
    circle [ cx (String.fromInt xCoord)
           , cy (String.fromInt yCoord)
           , r (String.fromInt radius)
           , fill "rgb(63, 143, 233)"
           , stroke "black"
           , strokeWidth "1"
           ] []

hands : Int -> Int -> Int -> List (Svg msg)
hands hour minute second =
    [ viewHand 4 hourHandRadius ((toFloat hour)/12)   "black"
    , viewHand 2 minHandRadius  ((toFloat minute)/60) "grey"
    , viewHand 1 secHandRadius  ((toFloat second)/60) "white"
    ]

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

hourMarks : List (Svg msg)
hourMarks =
    List.map (\n -> hourMark markRadius n) (List.range 1 12)

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

minMarks : List (Svg msg)
minMarks =
    List.map (\n -> minMark markRadius n) (List.range 1 60)

minMark : Float -> Int -> Svg msg
minMark length minute =
    let
        turns = (toFloat minute)/60
        x = coordX length turns
        y = coordY length turns
    in
    circle [ cx (String.fromFloat x)
           , cy (String.fromFloat y)
           , r "1"
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

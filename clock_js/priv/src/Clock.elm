module Clock exposing (clock)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)

boxSize = 400
xCoord  = boxSize/2
yCoord  = boxSize/2
radius  = boxSize/2 * 3/4

markRadius     = radius - (radius * 0.1)
hourHandRadius = radius - (radius * 0.5)
minHandRadius  = radius - (radius * 0.3)
secHandRadius  = radius - (radius * 0.2)

clock : Weekday -> Int -> Int -> Int -> Int -> Html msg
clock weekday day hour minute second =
    let
        handValues = hands hour minute second
        dateValues = date_ weekday day
    in
    svg [ viewBox (viewBoxValue boxSize)
        , width (String.fromInt boxSize)
        , height (String.fromInt boxSize)
        ]
    (List.concat [ background
                 , dateBox
                 , dateValues
                 , handValues
                 , hourMarks
                 , minMarks
                 ]
    )

viewBoxValue : Int -> String
viewBoxValue size =
    List.map (\n -> String.fromInt n) [0, 0, size, size]
        |> List.intersperse " "
        |> String.concat

background : List (Svg msg)
background =
    [ circle [ cx (String.fromFloat xCoord)
             , cy (String.fromFloat yCoord)
             , r  (String.fromFloat radius)
             , fill "lightgrey"
             , stroke "black"
             , strokeWidth "1"
             ] []
    ]

dateBox : List (Svg msg)
dateBox =
    [ rect [ x      (String.fromFloat (xCoord * 280/200))
           , y      (String.fromFloat (yCoord * 190/200))
           , width  (String.fromFloat (xCoord * 45/200))
           , height (String.fromFloat (yCoord * 20/200))
           , fill "darkgrey"
           , stroke "black"
           , strokeWidth "1"
           ] []
    ]

date_ : Weekday -> Int -> List (Svg msg)
date_ weekday day =
    [ weekday_ weekday
    , day_ day
    ]

weekday_ : Weekday -> Svg msg
weekday_ weekday =
    text_ [ x (String.fromFloat (xCoord * 283/200))
          , y (String.fromFloat (yCoord * 204/200))
          , fill (weekdayToColour weekday)
          , fontSize "12"
          ]
    [ text (weekdayToString weekday)
    ]

day_ : Int -> Svg msg
day_ day =
    text_ [ x (String.fromFloat (xCoord * 310/200))
          , y (String.fromFloat (yCoord * 204/200))
          , fill "white"
          , fontSize "12"
          ]
    [ text (String.fromInt day)
    ]

hands : Int -> Int -> Int -> List (Svg msg)
hands hour minute second =
    [ viewHand 4 hourHandRadius ((toFloat hour)/12)   "black"
    , viewHand 2 minHandRadius  ((toFloat minute)/60) "black"
    , viewHand 1 secHandRadius  ((toFloat second)/60) "white"
    ]

viewHand : Int -> Float -> Float -> String -> Svg msg
viewHand width length turns colour =
    let
        x = coordX length turns
        y = coordY length turns
    in
        line
        [ x1 (String.fromFloat xCoord)
        , y1 (String.fromFloat yCoord)
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
        xCoord + length * cos t

coordY : Float -> Float -> Float
coordY length turns =
    let
        t = 2 * pi * (turns - 0.25)
    in
        yCoord + length * sin t

weekdayToString : Weekday -> String
weekdayToString weekday =
    case weekday of
        Mon -> "Mon"
        Tue -> "Tue"
        Wed -> "Wed"
        Thu -> "Thu"
        Fri -> "Fri"
        Sat -> "Sat"
        Sun -> "Sun"

weekdayToColour : Weekday -> String
weekdayToColour weekday =
    case weekday of
        Mon -> "white"
        Tue -> "white"
        Wed -> "white"
        Thu -> "white"
        Fri -> "white"
        Sat -> "blue"
        Sun -> "red"

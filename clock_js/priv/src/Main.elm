module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, text, div, h1, p)
import Task
import Time exposing (..)

import Clock exposing (clock)

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

view : Model -> Html Msg
view model =
  let
    year    = Time.toYear    model.zone model.time
    month   = Time.toMonth   model.zone model.time
    day     = Time.toDay     model.zone model.time
    weekday = Time.toWeekday model.zone model.time
    hour    = Time.toHour    model.zone model.time
    minute  = Time.toMinute  model.zone model.time
    second  = Time.toSecond  model.zone model.time
  in
  div []
    [ h1 [] [ text "Date"]
    , p  [] [ text (weekdayToString weekday)
            , text " "
            , text (monthToString month)
            , text " "
            , text (String.fromInt day)
            , text "th  "
            , text (String.fromInt year)
            ]
    , h1 [] [ text "Digital Clock"]
    , p  [] [ text (String.fromInt hour   |> String.padLeft 2 '0')
            , text ":"
            , text (String.fromInt minute |> String.padLeft 2 '0')
            , text ":"
            , text (String.fromInt second |> String.padLeft 2 '0')
            ]
    , h1 [] [ text "Analogue Clock"]
    , clock hour minute second
    ]

monthToString : Month -> String
monthToString month =
    case month of
        Jan -> "January"
        Feb -> "February"
        Mar -> "March"
        Apr -> "April"
        May -> "May"
        Jun -> "June"
        Jul -> "July"
        Aug -> "August"
        Sep -> "September"
        Oct -> "October"
        Nov -> "November"
        Dec -> "December"

weekdayToString : Weekday -> String
weekdayToString weekday =
    case weekday of
        Mon -> "Monday"
        Tue -> "Tuesday"
        Wed -> "Wednesday"
        Thu -> "Thursday"
        Fri -> "Friday"
        Sat -> "Saturday"
        Sun -> "Sunday"

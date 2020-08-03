module Example.TimeTest exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time exposing (Month(..))
import TimeUtils
import Round

import Moon

import Date



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
  , Task.perform AdjustTimeZone Time.here
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

-- Lunar View

viewMoonDay : Time.Zone -> Moon.MoonDay -> String
viewMoonDay zone moon =
    "{ day : " ++ String.fromInt moon.day ++ ", start : " ++ (viewTime zone moon.startTime) ++
    ", end : " ++ (viewTime zone moon.endTime) ++ ", phase : " ++ (Round.round 2 moon.phase) ++ " }"

viewDayInfo : Time.Zone -> Moon.DayInfo -> Html Msg
viewDayInfo zone dayinfo =
    div [ class "dayinfo"]
        [ Date.toIsoString dayinfo.date |> ptext 
        , div [ class "mooninfo" ] <| List.map (viewMoonDay zone >> ptext) dayinfo.moonInfo
        ]


-- VIEW


ptext : String -> Html Msg
ptext t = p [] [text t] 


viewTime zone time =
  let
    hour   = String.fromInt (Time.toHour   zone time)
    minute = String.fromInt (Time.toMinute zone time)
    second = String.fromInt (Time.toSecond zone time)
    day    = String.fromInt (Time.toDay    zone time)
    month  = Time.toMonth zone time |> Date.monthToNumber |> String.fromInt
    year   = String.fromInt (Time.toYear   zone time)         
  in
     day ++ "." ++ month ++ "." ++ year ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second 


view : Model -> Html Msg
view model =
    let 
        start = TimeUtils.halfMonthBack    model.zone model.time
        end   = TimeUtils.halfMonthForward model.zone model.time
    in
        div [] <| List.map (viewDayInfo model.zone)
                           (Moon.lunarCalendar model.zone { latitude  = 56.142406
                                                          , longitude = 37.440216 }
                                               start end)

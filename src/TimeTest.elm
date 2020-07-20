module TimeTest exposing (..)


import Browser
import Html exposing (..)
import Task
import Time exposing (Month(..))
import TimeUtils exposing (halfMonthBack, halfMonthForward)

import Moon exposing (lunarDaysByPeriod, MoonDayInfo)


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

viewMoonDay : Time.Zone -> MoonDayInfo -> String
viewMoonDay zone moon =
    "{ day : " ++ String.fromInt moon.day ++ ", rise : " ++ (viewTime zone moon.riseTime) ++
    ", set : " ++ (viewTime zone moon.setTime) ++ " }"


-- VIEW



ptext = \t -> p [] [text t] 

toEnglishMonth : Month -> String
toEnglishMonth month =
      case month of
        Jan -> "january"
        Feb -> "february"
        Mar -> "march"
        Apr -> "april"
        May -> "may"
        Jun -> "june"
        Jul -> "july"
        Aug -> "august"
        Sep -> "september"
        Oct -> "october"
        Nov -> "november"
        Dec -> "december"


viewTime zone time =
  let
    hour   = String.fromInt (Time.toHour   zone time)
    minute = String.fromInt (Time.toMinute zone time)
    second = String.fromInt (Time.toSecond zone time)
    day    = String.fromInt (Time.toDay    zone time)
    month  = toEnglishMonth (Time.toMonth  zone time)
    year   = String.fromInt (Time.toYear   zone time)         
  in
     day ++ "." ++ month ++ "." ++ year ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second 


view : Model -> Html Msg
view model =
    let 
        start = halfMonthBack    model.zone model.time
        end   = halfMonthForward model.zone model.time
    in
        div [] <| List.map (viewMoonDay model.zone >> ptext)
                           (lunarDaysByPeriod model.zone { latitude  = 56.142406
                                                         , longitude = 37.440216 }
                                              start end)

module MoonInfo exposing (..)

import Time exposing (Posix)

import List exposing (map, range)

import DaysSince2000 exposing (DaysSince2000(..), dayStart, shift, fromPosix, diff)
import SunCalc exposing (moonIlluminationByDays, moonDay)

type alias MoonInfo =
    List MoonDayInfo

type alias MoonDayInfo =
    { day : Int
    , phase : Float
    -- , riseDate : Date --
    -- , riseTime : Int --
    -- , fromTime : Int  -- ??
    }


-- It's not correct now, needs to be improved --
moonInfoByDay : DaysSince2000 -> MoonInfo
moonInfoByDay dayTime =
    let
        sm = dayStart dayTime |> moonIlluminationByDays
        em = moonIlluminationByDays <| (dayStart dayTime |> shift) <| 1
    in
        [ { day = moonDay sm 
          , phase = sm.phase }
        , { day = moonDay em
          , phase = em.phase } ]

moonInfo : Posix -> MoonInfo
moonInfo = moonInfoByDay << DaysSince2000.fromPosix


-- Intervals  --

type Interval = Interval DaysSince2000 DaysSince2000

iterate : Interval -> List DaysSince2000
iterate (Interval start end) = 
    range 0 (diff start end |> floor) |> map (shift start)

around30 : Posix -> Interval
around30 = fromPosix >> dayStart >> (\d -> Interval (shift d -30) (shift d 30))

-- Utility --

moonInfoAround30 : Posix -> List MoonInfo
moonInfoAround30 = around30 >> iterate >> map moonInfoByDay

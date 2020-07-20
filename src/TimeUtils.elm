module TimeUtils exposing (..)

import Time exposing (Posix)
import Time.Extra exposing (Interval(..), add, diff, floor, ceiling)

startOfDay = Time.Extra.floor Day
endOfDay   = Time.Extra.ceiling Day
addHours   = add Hour
addMinutes = add Minute

previousDay = add Day -1
nextDay     = add Day 1

halfMonthBack    = add Day -15
halfMonthForward = add Day  15

intervals : Interval -> Time.Zone -> Posix -> Posix -> List Posix
intervals interval zone start end =
    let
        startDay = startOfDay zone start
        endDay   = endOfDay zone end
        total    = diff interval zone startDay endDay
    in
        List.map (\i -> add interval i zone startDay) <| List.range 0 total

hourIntervals = intervals Hour
dayIntervals  = intervals Day



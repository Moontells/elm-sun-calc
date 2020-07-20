module TimeUtils exposing (..)

import Time exposing (Posix)
import Time.Extra exposing (Interval(..), add, diff, floor, ceiling)


startOfDay = Time.Extra.floor Day
endOfDay   = Time.Extra.ceiling Day
addHours   = add Hour

hourIntervals : Time.Zone -> Posix -> Posix -> List Posix
hourIntervals zone start end =
    let
        startDay = startOfDay zone start
        endDay   = endOfDay zone end
        totalHours = diff Hour zone startDay endDay
    in
        List.map (\i -> add Hour i zone startDay) <| List.range 0 totalHours

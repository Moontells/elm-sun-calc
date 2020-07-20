module Moon exposing (..)

import Time exposing (Posix)
import TimeUtils exposing (..)

import List exposing (map, range, indexedMap)

import SunCalc exposing (CrossResult, moonIllumination, crossHorizon,
                         moonAltitude, addFloatHours, Coordinated)

import DaysSince2000 exposing (defaultTime)

type alias MoonInfo =
    { day : Int -- Date --
    , phase : Float  
    , moon_days : List MoonDayInfo
    }


type alias MoonDayInfo =
    { day : Int
    , riseTime : Posix
    , setTime  : Posix
    -- , riseDate : Date --
    -- , setDate  : Date --
    }


startOfLunarMonth : Time.Zone -> Posix -> Posix
startOfLunarMonth zone startTime = 
    let
        iterate last_phase time =
            let new_phase = moonIllumination time |> .phase
            in case last_phase of
                Nothing ->
                    iterate (Just new_phase) <| previousDay zone time
                Just phase ->
                    if new_phase > phase
                    then time
                    else
                        iterate (Just new_phase) <| previousDay zone time
    in
        iterate Nothing startTime


lunarDaysByPeriod : Time.Zone -> Coordinated {} -> Posix -> Posix -> List MoonDayInfo
lunarDaysByPeriod zone coord start end =
    let 
        new_start = startOfLunarMonth zone start
        hours = hourIntervals zone new_start end
        calcCrossHorizon time =
            crossHorizon 0
                    (moonAltitude (addMinutes -30 zone time) coord)
                    (moonAltitude time coord)
                    (moonAltitude (addMinutes 30 zone time) coord)
                    (CrossResult Nothing Nothing) |> baseAt time
    in
        map calcCrossHorizon hours |> List.reverse |> composeToLunarDays |> indexedMap toMoonDay


composeToLunarDays : List PosixCrossResult -> List PosixCrossResult
composeToLunarDays crosses =
    let
        iterate new collected =
            case new of
                [] ->
                    collected
                current :: remaining ->
                    iterate remaining <| batch current collected
    in
        iterate crosses []
        

batch : PosixCrossResult -> List PosixCrossResult -> List PosixCrossResult
batch next collected =
    case collected of
        [] ->
            List.singleton next
        current :: previous ->
            case next.setTime of
                Nothing ->
                    case next.riseTime of
                        Nothing ->
                            collected
                        Just nextRiseTime ->
                            case current.riseTime of
                                Nothing ->
                                    { current | riseTime = Just nextRiseTime } :: previous
                                Just currentRiseTime ->
                                    -- WTF ERROR --
                                    next :: collected
                Just time ->
                    next :: collected
                

type alias PosixCrossResult =
    { riseTime : Maybe Posix
    , setTime  : Maybe Posix
    }


baseAt : Posix -> CrossResult -> PosixCrossResult
baseAt posix cross =
    let 
        baseMaybe = Maybe.map (\offset -> addFloatHours offset posix)
    in
        { riseTime = baseMaybe cross.riseOffset
        , setTime  = baseMaybe cross.setOffset
        }


toMoonDay : Int -> PosixCrossResult -> MoonDayInfo
toMoonDay i posixCross =
    { day = i
    , riseTime = posixCross.riseTime |> Maybe.withDefault defaultTime
    , setTime  = posixCross.setTime  |> Maybe.withDefault defaultTime
    }

{-
moonInfoByPeriod : Time.Zone -> Coordinated {} -> Posix -> Posix -> List MoonInfo
moonInfoByPeriod tzone coord start end =
-- days  = dayIntervals zone new_start end --
-}  

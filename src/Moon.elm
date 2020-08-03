module Moon exposing (..)

import Time exposing (Posix, posixToMillis, millisToPosix)
import TimeUtils exposing (..)
import Date exposing (Date, fromPosix)

import List exposing (map, range, filter, filterMap, append)

import SunCalc exposing (CrossResult, Coordinated)


-- Calculations of lunar months  --

previousNewMoon : Time.Zone -> Posix -> Posix
previousNewMoon zone startTime = 
    let
        iterate last_phase time =
            let new_phase = SunCalc.moonIllumination time |> .phase
            in case last_phase of
                Nothing ->
                    iterate (Just new_phase) <| previousDay zone time
                Just phase ->
                    if new_phase > phase
                    then binsearchNewMoon time (nextDay zone time)
                    else
                        iterate (Just new_phase) <| previousDay zone time
    in
        iterate Nothing startTime


binsearchNewMoon : Posix -> Posix -> Posix
binsearchNewMoon left right =
    let
        offsetToPosix = floor >> (+) (posixToMillis left) >> millisToPosix
        phase = offsetToPosix >> SunCalc.moonIllumination >> .phase
        search i l r = 
            let
                m = (l+r)/2
            in if i <= 0
            then m
            else if phase m > 0.5
               then search (i-1) m r
               else search (i-1) l m
        startOffset = (posixToMillis right) - (posixToMillis left) |> toFloat
    in 
        search 20 0 startOffset |> offsetToPosix -- 20 iterations give subsecond precision


newMoonsInInterval : Time.Zone -> Posix -> Posix -> List Posix
newMoonsInInterval zone start end =
    let moons current =
            if (before current start) then []
            else current :: ( moons <| previousNewMoon zone <| addHours -1 zone current )
    in moons end |> List.reverse


-- Calculations of lunar days --

lunarDaysByPeriod : Time.Zone -> Coordinated {} -> Posix -> Posix -> List MoonDay
lunarDaysByPeriod zone coord intervalStart intervalEnd =
    let 
        startOfLunarMonth = previousNewMoon zone intervalStart
        newMoons = newMoonsInInterval zone intervalStart intervalEnd
        altitude time offset = SunCalc.moonAltitude (addMinutes offset zone time) coord
        partialLunarDays time =
            SunCalc.crossHorizon 0
                    (altitude time -30)
                    (altitude time 0)
                    (altitude time 30)
                    (CrossResult Nothing Nothing) |> baseAt time
    in
        hourIntervals zone startOfLunarMonth (addHours 25 zone intervalEnd)
            |> map partialLunarDays
            |> map notBeginningOfLunarMonth
            |> insertNewMoons (startOfLunarMonth :: newMoons)
            |> composeLunarDays
            |> enumerateMoonDays
            |> filterMap nonmaybe
            |> map (toMoonDay coord)
            |> trimByInterval intervalStart intervalEnd


type Interval b = Interval b b

type Temporal a b = Temporal (Interval b) a


nonmaybe : Temporal a (Maybe b) -> Maybe (Temporal a b)
nonmaybe (Temporal interval additional) =
    case interval of
        (Interval (Just x) (Just y)) ->
            Just <| Temporal (Interval x y) additional
        _ ->
            Nothing


baseAt : Posix -> CrossResult -> Temporal {} (Maybe Posix)
baseAt posix cross =
    let 
        base = Maybe.map (\offset -> TimeUtils.addFloatHours offset posix)
    in
        Temporal (Interval (base cross.riseOffset) Nothing) {}


insertNewMoons : List Posix -> List (Temporal {lunarMonthStart : Bool} (Maybe Posix)) -> List (Temporal {lunarMonthStart : Bool} (Maybe Posix))
insertNewMoons newmoons lunardays =
    let 
        insertion time = Temporal (Interval (Just time) Nothing) {lunarMonthStart = True}
        
        iterate collected days moons =
            case ( days , moons ) of 
                ( current :: future_days , newMoonTime :: future_moons ) ->
                    let (Temporal (Interval dayStart _) _) = current
                    in case dayStart of 
                           Nothing -> iterate (current :: collected) future_days moons
                           Just dayStartTime -> if (before newMoonTime dayStartTime )
                                                then iterate ((insertion newMoonTime) :: collected) days future_moons
                                                else iterate (                current :: collected) future_days moons
                ( _  , [] ) -> append (List.reverse days) collected
                ( [] , _  ) -> collected
    in iterate [] lunardays newmoons |> List.reverse


composeLunarDays : List (Temporal a (Maybe Posix)) -> List (Temporal a (Maybe Posix))
composeLunarDays crosses =
    let
        iterate new collected =
            case new of
                [] -> collected
                current :: remaining ->
                    iterate remaining <| batch current collected
    in
        iterate crosses [] |> List.reverse


batch : Temporal a (Maybe some) -> List (Temporal a (Maybe some)) -> List (Temporal a (Maybe some))
batch next collected =
    case collected of
        [] ->
            List.singleton next
        (Temporal current_interval additional) :: previous ->
            let (Temporal next_interval _) = next
            in case (current_interval, next_interval) of
                ( _ , Interval Nothing Nothing) ->
                    collected
                ( Interval cs Nothing , Interval ns Nothing ) ->
                    next :: Temporal (Interval cs ns) additional :: previous
                _ ->
                    next :: collected


notBeginningOfLunarMonth : Temporal {} b -> Temporal {lunarMonthStart : Bool} b
notBeginningOfLunarMonth (Temporal interval _) =
    Temporal interval {lunarMonthStart = False}


enumerateMoonDays : List (Temporal {lunarMonthStart : Bool} b) -> List (Temporal {day : Int} b)
enumerateMoonDays lunardays =
    let numerate days index =
            case days of
                [] -> []
                Temporal interval dayinfo :: remaining ->
                          let daynumber = if dayinfo.lunarMonthStart then Just 1 else index
                          in case daynumber of
                                 Nothing -> numerate remaining Nothing
                                 Just n  -> Temporal interval { day = n } :: (numerate remaining <| Just (n + 1))
    in numerate lunardays Nothing


-- Moon Day transofrmations --

type alias MoonDay =
    { day : Int
    , phase : Float
    , startTime : Posix
    , endTime   : Posix
    }


toMoonDay : Coordinated {} -> Temporal { day : Int } Posix -> MoonDay
toMoonDay coord (Temporal (Interval start end) dayinfo) =
    { day = dayinfo.day
    , startTime = start
    , endTime   = end
    , phase = if dayinfo.day == 1 then 0 else SunCalc.moonIllumination start |> .phase
    }


trimByInterval : Posix -> Posix -> List MoonDay -> List MoonDay
trimByInterval start end =
    let isInInterval moonday = (before moonday.startTime end) && (before start moonday.endTime)
    in filter isInInterval


type alias DayInfo =
    { date : Date
    , moonInfo : List MoonDay
    }


lunarCalendar : Time.Zone -> Coordinated {} -> Posix -> Posix -> List DayInfo
lunarCalendar zone coord start end = lunarDaysByPeriod zone coord start end
                                     |> arrangeByDays zone start end

arrangeByDays : Time.Zone -> Posix -> Posix -> List MoonDay -> List DayInfo
arrangeByDays zone start end moondays = dayIntervals zone start end
                                      |> map (\t -> ( (addHours 3 zone t) , trimByInterval t (addHours 24 zone t) moondays ) )
                                      |> map (\(time, moonInfo) -> { date = Date.fromPosix zone time 
                                                                   , moonInfo = moonInfo})

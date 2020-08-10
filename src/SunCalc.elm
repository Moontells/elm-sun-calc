module SunCalc exposing
    ( Location
    , SunPosition, sunPosition
    , morningSunTime, sunrise, sunriseEnd, dawn, nauticalDawn, nightEnd, goldenHourEnd
    , eveningSunTime, sunset, sunsetStart, dusk, nauticalDusk, night, golderHour
    , MoonIllumination, moonIllumination, MoonPosition, moonPosition, MoonTimes(..), moonTimes
    )

{-| This module provides functionality for calculating sun/moon position and light phases.
This is a port of Vladimir Agafonkin's [SunCalc JavaScript library](https://github.com/mourner/suncalc).


# Location

@docs Location


# Sun position

@docs SunPosition, sunPosition


# Morning sun time

@docs morningSunTime, sunrise, sunriseEnd, dawn, nauticalDawn, nightEnd, goldenHourEnd


# Evening sun time

@docs eveningSunTime, sunset, sunsetStart, dusk, nauticalDusk, night, golderHour


# Moon

@docs MoonIllumination, moonIllumination, MoonPosition, moonPosition, MoonTimes, moonTimes

-}

import DaysSince2000 exposing (DaysSince2000)
import Time
import Time.Extra as Time



-- LOCATION


{-| Geographic location. Extensible record is used for convenience to avoid unnecessary transformations.
-}
type alias Location a =
    { a
        | latitude : Float
        , longitude : Float
    }



-- SUN COMPUTATIONS


{-| Sun position
-}
type alias SunPosition =
    { azimuth : Float
    , altitude : Float
    }


{-| Calculates sun position for given time and location
-}
sunPosition : Time.Posix -> Location a -> SunPosition
sunPosition posix location =
    let
        latitude =
            degrees -location.longitude

        longitude =
            degrees location.latitude

        days =
            DaysSince2000.fromPosix posix

        equatorialLocation =
            sunLocation days

        hourAngle =
            siderealTime days latitude - equatorialLocation.rightAscension
    in
    { azimuth = azimuth hourAngle longitude equatorialLocation.declination
    , altitude = altitude hourAngle longitude equatorialLocation.declination
    }


{-| Calculates solar [mean anomaly](https://en.wikipedia.org/wiki/Mean_anomaly) for Earth in radians
-}
earthSolarMeanAnomaly : DaysSince2000 -> Float
earthSolarMeanAnomaly days =
    degrees ((earthM1 * DaysSince2000.unwrap days) + earthM0)


{-| Calculates [equation of the center](https://en.wikipedia.org/wiki/Equation_of_the_center) for Earth.
The computation uses an approximation of the shape

`C ≈ c1 sin(M) + c2 sin(2M) + c3 sin(3M)`

where `M` is the Mean Anomaly and `ci` are constants, as given on this page
[this page](https://www.aa.quae.nl/en/reken/zonpositie.html#3).

-}
earthEquationOfCenter : Float -> Float
earthEquationOfCenter earthSMA =
    let
        c1 =
            1.9148

        c2 =
            0.02

        c3 =
            0.0003
    in
    degrees ((c1 * sin earthSMA) + (c2 * sin (2 * earthSMA)) + (c3 * sin (3 * earthSMA)))


{-| Calculates [ecliptic](https://en.wikipedia.org/wiki/Ecliptic_coordinate_system) longitude for Earth
-}
earthEclipticLongitude : Float -> Float
earthEclipticLongitude earthSMA =
    earthSMA + earthEquationOfCenter earthSMA + earthPerihelion + pi


{-| Geocentric ecliptic location of the Sun
-}
type alias SunLocation =
    { declination : Float
    , rightAscension : Float
    }


sunLocation : DaysSince2000 -> SunLocation
sunLocation days =
    let
        earthSMA =
            earthSolarMeanAnomaly days

        earthEL =
            earthEclipticLongitude earthSMA
    in
    { declination = earthDeclination earthEL sunEclipticLatitude
    , rightAscension = earthRightAscension earthEL sunEclipticLatitude
    }



-- SUN TIME COMPUTATIONS


type SunTime
    = Morning
    | Evening


sunTime : SunTime -> Float -> Time.Posix -> Location a -> Float -> Time.Posix
sunTime time angle posix location height =
    let
        latitude =
            degrees -location.longitude

        longitude =
            degrees location.latitude

        days =
            DaysSince2000.fromPosix posix

        observerAngle =
            -2.076 * sqrt height / 60

        julianCycle =
            DaysSince2000.map (\ds -> toFloat (round (ds - j0 - latitude / (2 * pi)))) days

        approxTransit =
            computeApproxTransit 0 latitude julianCycle

        solarMeanAnomaly =
            earthSolarMeanAnomaly approxTransit

        eclipticLongitude =
            earthEclipticLongitude solarMeanAnomaly

        declination =
            earthDeclination eclipticLongitude 0

        noon =
            solarTransit approxTransit solarMeanAnomaly eclipticLongitude

        actualAngle =
            degrees (angle + observerAngle)

        hourAngle =
            acos ((sin actualAngle - sin longitude * sin declination) / (cos longitude * cos declination))

        set =
            solarTransit
                (computeApproxTransit hourAngle latitude julianCycle)
                solarMeanAnomaly
                eclipticLongitude
    in
    case time of
        Morning ->
            DaysSince2000.map2 (\dsSet dsNoon -> 2 * dsNoon - dsSet) set noon
                |> DaysSince2000.toPosix

        Evening ->
            DaysSince2000.toPosix set


solarTransit : DaysSince2000 -> Float -> Float -> DaysSince2000
solarTransit days solarMeanAnomaly eclipticLongitude =
    DaysSince2000.map (\ds -> ds + 0.0053 * sin solarMeanAnomaly - 0.0069 * sin (2 * eclipticLongitude)) days


computeApproxTransit : Float -> Float -> DaysSince2000 -> DaysSince2000
computeApproxTransit hourAngle longitude julianCycle =
    DaysSince2000.map (\ds -> j0 + (hourAngle + longitude) / (2 * pi) + ds) julianCycle



-- MORNING SUN TIME COMPUTATIONS


{-| Calculates the time where the sun passes at a given angle in the morning.

Example:

    morningSunTime
        -0.833 -- angle
        now    -- current time
        location -- your coordinates

Use [sunrise](#sunrise), [sunriseEnd](#sunriseEnd), etc.. for predefined angles.

-}
morningSunTime : Float -> Time.Posix -> Location a -> Time.Posix
morningSunTime angle posix location =
    sunTime Morning angle posix location 0


{-| Calculates the time for the sunrise. See also [morningSunTime](#morningSunTime).
Sunrise - top edge of the sun appears on the horizon.
-}
sunrise : Time.Posix -> Location a -> Time.Posix
sunrise =
    morningSunTime -0.833


{-| Calculates the time for the end of the sunrise. See also [morningSunTime](#morningSunTime).
Sunrise ends - bottom edge of the sun touches the horizon.
-}
sunriseEnd : Time.Posix -> Location a -> Time.Posix
sunriseEnd =
    morningSunTime -0.3


{-| Calculates the time for the dawn. See also [morningSunTime](#morningSunTime).
Dawn - morning nautical twilight ends, morning civil twilight starts.
-}
dawn : Time.Posix -> Location a -> Time.Posix
dawn =
    morningSunTime -6


{-| Calculates the time for the nautical dawn. See also [morningSunTime](#morningSunTime).
Nautical dawn - morning nautical twilight starts.
-}
nauticalDawn : Time.Posix -> Location a -> Time.Posix
nauticalDawn =
    morningSunTime -12


{-| Calculates the time for the night end. See also [morningSunTime](#morningSunTime).
Night ends - morning astronomical twilight starts.
-}
nightEnd : Time.Posix -> Location a -> Time.Posix
nightEnd =
    morningSunTime -18


{-| Calculates the time for the golden hour end. See also [morningSunTime](#morningSunTime).
Morning golden hour - soft light, best time for photography.
-}
goldenHourEnd : Time.Posix -> Location a -> Time.Posix
goldenHourEnd =
    morningSunTime 6



-- EVENING SUN TIME COMPUTATIONS


{-| Calculates the time where the sun passes at a given angle in the evening.

Example:

    eveningSunTime
        -0.833 -- angle
        now    -- current time
        location -- your coordinates

Use [sunset](#sunset) or [sunsetStart](#sunsetStart) for predefined angles.

-}
eveningSunTime : Float -> Time.Posix -> Location a -> Time.Posix
eveningSunTime angle posix location =
    sunTime Evening angle posix location 0


{-| Calculates the time for the sunset. See also [eveningSunTime](#eveningSunTime).
Sunset - sun disappears below the horizon, evening civil twilight starts.
-}
sunset : Time.Posix -> Location a -> Time.Posix
sunset =
    eveningSunTime -0.833


{-| Calculates the time for the begning of the sunset. See also [eveningSunTime](#eveningSunTime).
Sunset starts - bottom edge of the sun touches the horizon.
-}
sunsetStart : Time.Posix -> Location a -> Time.Posix
sunsetStart =
    eveningSunTime -0.3


{-| Calculates the time for the dusk. See also [eveningSunTime](#eveningSunTime).
Dusk - evening nautical twilight starts.
-}
dusk : Time.Posix -> Location a -> Time.Posix
dusk =
    morningSunTime -6


{-| Calculates the time for the nautical dusk. See also [eveningSunTime](#eveningSunTime).
Nautical dusk - evening astronomical twilight starts.
-}
nauticalDusk : Time.Posix -> Location a -> Time.Posix
nauticalDusk =
    morningSunTime -12


{-| Calculates the time for the beginning of the night. See also [eveningSunTime](#eveningSunTime).
Night starts - dark enough for astronomical observations.
-}
night : Time.Posix -> Location a -> Time.Posix
night =
    morningSunTime -18


{-| Calculates the time for the beginning of the golden hour. See also [eveningSunTime](#eveningSunTime).
Evening golden hour - soft light, best time for photography.
-}
golderHour : Time.Posix -> Location a -> Time.Posix
golderHour =
    morningSunTime 6



-- MOON COMPUTATIONS


{-| Moon position
-}
type alias MoonPosition =
    { azimuth : Float
    , altitude : Float
    , distance : Float
    , parallacticAngle : Float
    }


{-| Calculates the position of the moon at a given time and location
-}
moonPosition : Time.Posix -> Location a -> MoonPosition
moonPosition posix location =
    let
        latitude =
            degrees -location.longitude

        longitude =
            degrees location.latitude

        days =
            DaysSince2000.fromPosix posix

        equatorialLocation =
            moonLocation days

        hourAngle =
            siderealTime days latitude - equatorialLocation.rightAscension

        actualAltitude =
            altitude hourAngle longitude equatorialLocation.declination

        -- formula 14.1 of "Astronomical Algorithms" 2nd edition by Jean Meeus (Willmann-Bell, Richmond) 1998.
        parallacticAngle =
            atan2
                (sin hourAngle)
                (tan longitude * cos equatorialLocation.declination - sin equatorialLocation.declination * cos hourAngle)
    in
    { azimuth = azimuth hourAngle longitude equatorialLocation.declination
    , altitude = actualAltitude + astroRefraction actualAltitude
    , distance = equatorialLocation.distance
    , parallacticAngle = parallacticAngle
    }


{-| Moon illumination
-}
type alias MoonIllumination =
    { fraction : Float
    , phase : Float
    , angle : Float
    }


{-| Calculates moon illumination at a given time.
By subtracting the `parallacticAngle` from the `angle` one can get the zenith angle of the moons bright limb (anticlockwise).
The zenith angle can be used do draw the moon shape from the observers perspective (e.g. moon lying on its back).
-}
moonIllumination : Time.Posix -> MoonIllumination
moonIllumination posix =
    let
        days =
            DaysSince2000.fromPosix posix

        sun =
            sunLocation days

        moon =
            moonLocation days

        geocentricElongation =
            acos (sin sun.declination * sin moon.declination + cos sun.declination * cos moon.declination * cos (sun.rightAscension - moon.rightAscension))

        selenoCentricElongation =
            atan2
                (earthSunDist * sin geocentricElongation)
                (moon.distance - earthSunDist * cos geocentricElongation)

        angle =
            atan2
                (cos sun.declination * sin (sun.rightAscension - moon.rightAscension))
                (sin sun.declination * cos moon.declination - cos sun.declination * sin moon.declination * cos (sun.rightAscension - moon.rightAscension))

        upOrDown =
            if angle < 0 then
                -1

            else
                1
    in
    { fraction = (1 + cos selenoCentricElongation) / 2
    , phase = 0.5 + 0.5 * selenoCentricElongation * upOrDown / pi
    , angle = angle
    }


{-| Geocentric ecliptic location of the Moon
-}
type alias MoonLocation =
    { declination : Float
    , rightAscension : Float
    , distance : Float
    }


moonLocation : DaysSince2000 -> MoonLocation
moonLocation days =
    let
        eclipticLongitude =
            degrees (218.316 + 13.176396 * DaysSince2000.unwrap days)

        meanAnomaly =
            degrees (134.963 + 13.064993 * DaysSince2000.unwrap days)

        meanDistance =
            degrees (93.272 + 13.22935 * DaysSince2000.unwrap days)

        longitude =
            eclipticLongitude + degrees 6.289 * sin meanAnomaly

        latitude =
            degrees 5.128 * sin meanDistance
    in
    { rightAscension = earthRightAscension longitude latitude
    , declination = earthDeclination longitude latitude
    , distance = 385001 - 20905 * cos meanAnomaly
    }


astroRefraction : Float -> Float
astroRefraction alti =
    -- The following formula works for positive altitudes only.
    -- If h = -0.08901179 a div/0 would occur.
    if alti < 0 then
        astroRefraction 0

    else
        0.0002967 / tan (alti + 0.00312536 / (alti + 0.08901179))



-- MOON TIMES COMPUTATIONS


{-| Moon rise and set times
-}
type MoonTimes
    = RiseAt Time.Posix
    | SetAt Time.Posix
    | RiseAndSetAt Time.Posix Time.Posix
    | AlwaysUp
    | AlwaysDown


{-| Calculates the times of the Moon rise and set (if any) for given time and location
-}
moonTimes : Time.Zone -> Time.Posix -> Location a -> MoonTimes
moonTimes zone posix location =
    let
        midnight =
            Time.startOfDay zone posix

        initialHeight =
            moonAltitude midnight location
    in
    loopCrossHorizon 1 midnight location (CrossResult Nothing Nothing)
        |> toMoonTimes initialHeight midnight


loopCrossHorizon : Int -> Time.Posix -> Location a -> CrossResult -> CrossResult
loopCrossHorizon offset midnight location currentResult =
    if offset > 24 then
        currentResult

    else
        let
            newResult =
                crossHorizon (toFloat offset)
                    (moonAltitude (Time.addHours (offset - 1) midnight) location)
                    (moonAltitude (Time.addHours offset midnight) location)
                    (moonAltitude (Time.addHours (offset + 1) midnight) location)
                    currentResult
        in
        if isEnded newResult then
            newResult

        else
            loopCrossHorizon (offset + 2) midnight location newResult


moonAltitude : Time.Posix -> Location a -> Float
moonAltitude date location =
    moonPosition date location
        |> .altitude
        -- altitude correction (why ???)
        |> (-) (degrees 0.133)


type alias CrossResult =
    { riseOffset : Maybe Float
    , setOffset : Maybe Float
    }


crossHorizon : Float -> Float -> Float -> Float -> CrossResult -> CrossResult
crossHorizon offset altBefore altCurrent altAfter currentResult =
    let
        a =
            (altBefore + altAfter) / 2 - altCurrent

        b =
            (altAfter - altBefore) / 2

        xe =
            -b / (2 * a)

        ye =
            (a * xe + b) * xe + altCurrent

        delta =
            b * b - 4 * a * altCurrent
    in
    if delta < 0 then
        currentResult

    else
        let
            dx =
                sqrt delta / (abs a * 2)

            x1_ =
                xe - dx

            x2 =
                xe + dx

            nbRoots =
                (if abs x1_ <= 1 then
                    1

                 else
                    0
                )
                    + (if abs x2 <= 1 then
                        1

                       else
                        0
                      )

            x1 =
                if x1_ < -1 then
                    x2

                else
                    x1_
        in
        case nbRoots of
            1 ->
                if altBefore < 0 then
                    { currentResult | riseOffset = Just (offset + x1) }

                else
                    { currentResult | setOffset = Just (offset + x1) }

            2 ->
                if ye < 0 then
                    { riseOffset = Just (offset + x2)
                    , setOffset = Just (offset + x1)
                    }

                else
                    { riseOffset = Just (offset + x1)
                    , setOffset = Just (offset + x2)
                    }

            _ ->
                currentResult


isEnded : CrossResult -> Bool
isEnded { riseOffset, setOffset } =
    riseOffset /= Nothing && setOffset /= Nothing


toMoonTimes : Float -> Time.Posix -> CrossResult -> MoonTimes
toMoonTimes height midnight { riseOffset, setOffset } =
    case ( riseOffset, setOffset ) of
        ( Nothing, Nothing ) ->
            if height > 0 then
                AlwaysUp

            else
                AlwaysDown

        ( Nothing, Just set ) ->
            SetAt (addHours set midnight)

        ( Just rise, Nothing ) ->
            RiseAt (addHours rise midnight)

        ( Just rise, Just set ) ->
            RiseAndSetAt (addHours rise midnight) (addHours set midnight)


addHours : Float -> Time.Posix -> Time.Posix
addHours dt posix =
    posix
        |> Time.posixToMillis
        |> (+) (round (dt * 3600000))
        |> Time.millisToPosix



-- GENERAL CALCULATIONS FOR POSITION


{-| Calculates [declination](https://en.wikipedia.org/wiki/Declination) for Earth
-}
earthDeclination : Float -> Float -> Float
earthDeclination eclipticLongitude eclipticLatitude =
    asin ((sin eclipticLatitude * cos earthObliquity) + (cos eclipticLatitude * sin earthObliquity * sin eclipticLongitude))


{-| Calculates [right ascension](https://en.wikipedia.org/wiki/Right_ascension) for Earth
-}
earthRightAscension : Float -> Float -> Float
earthRightAscension eclipticLongitude eclipticLatitude =
    atan2
        ((sin eclipticLongitude * cos earthObliquity) - (tan eclipticLatitude * sin earthObliquity))
        (cos eclipticLongitude)


{-| Calculates [sidereal time](https://en.wikipedia.org/wiki/Sidereal_time) for Earth
-}
siderealTime : DaysSince2000 -> Float -> Float
siderealTime days latitude =
    (earthSiderealTimeJulian2000 + earthSiderealTimeChangeRate * DaysSince2000.unwrap days)
        |> degrees
        |> (\a -> a - latitude)


{-| Calculates [azimuth](https://en.wikipedia.org/wiki/Azimuth)
-}
azimuth : Float -> Float -> Float -> Float
azimuth hourAngle longitude declination =
    atan2
        (sin hourAngle)
        ((cos hourAngle * sin longitude) - (tan declination * cos longitude))


{-| Calculates [altitude](https://en.wikipedia.org/wiki/Altitude)
-}
altitude : Float -> Float -> Float -> Float
altitude hourAngle longitude declination =
    asin ((sin longitude * sin declination) + (cos longitude * cos declination * cos hourAngle))



-- CONSTANTS


{-| Constant needed for Sun time computation (what does it stand for???)
-}
j0 : Float
j0 =
    0.0009


{-| Earth [mean anomaly](https://en.wikipedia.org/wiki/Mean_anomaly)
for [Julian day number](https://en.wikipedia.org/wiki/Julian_day) of year 2000
-}
earthM0 : Float
earthM0 =
    357.5291


{-| Earth angular [mean motion](https://en.wikipedia.org/wiki/Mean_motion)
-}
earthM1 : Float
earthM1 =
    0.98560028


{-| [Perihelion](https://en.wikipedia.org/wiki/Perihelion_and_aphelion) for Earth
-}
earthPerihelion : Float
earthPerihelion =
    degrees 102.9373


{-| [Obliquity](https://www.obliquity.com/info/meaning.html) for Earth
-}
earthObliquity : Float
earthObliquity =
    degrees 23.4393


{-| Earth [sidereal time](https://en.wikipedia.org/wiki/Sidereal_time) (in degrees) at longitude 0° at the instant
defined by [Julian Day](https://en.wikipedia.org/wiki/Julian_day) of year 2000
-}
earthSiderealTimeJulian2000 : Float
earthSiderealTimeJulian2000 =
    280.16


{-| Earth rate of change of the [sidereal time](https://en.wikipedia.org/wiki/Sidereal_time)
-}
earthSiderealTimeChangeRate : Float
earthSiderealTimeChangeRate =
    360.9856235


{-| Distance in km between the earth and the sun
-}
earthSunDist : Float
earthSunDist =
    149598000


{-| [Ecliptic]((https://en.wikipedia.org/wiki/Ecliptic_coordinate_system)) latitude for Sun
-}
sunEclipticLatitude : Float
sunEclipticLatitude =
    0

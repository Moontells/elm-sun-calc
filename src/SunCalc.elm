module SunCalc exposing
    ( Coordinated, Positioned
    , sunPosition, riseTime, setTime, sunrise, sunriseEnd, sunset, sunsetStart
    , MoonIllumination, moonIllumination, moonIlluminationByDays,  moonPosition
    , MoonTimes(..), moonTimes
    , moonDay
    )

{-| This library provides functionality for calculating sun/moon position and light phases.
This is a port of Vladimir Agafonkin's [SunCalc JavaScript library](https://github.com/mourner/suncalc)

The `Posix` and `Zone` types are the ones defined in [elm/time](https://package.elm-lang.org/packages/elm/time/latest).


# Coordinates systems

@docs Coordinated, Positioned


# Sun

@docs sunPosition, riseTime, setTime, sunrise, sunriseEnd, sunset, sunsetStart


# Moon

@docs MoonIllumination, moonIllumination, moonPosition, MoonTimes, moonTimes

-}

import DaysSince2000 exposing (DaysSince2000, unwrap)
import Time exposing (Posix)
import Time.Extra


{-| -}
type alias Coordinated a =
    { a
        | latitude : Float
        , longitude : Float
    }


{-| -}
type alias Positioned a =
    { a
        | azimuth : Float
        , altitude : Float
    }



-- SUN COMPUTATIONS


{-| Calculates sun position for given time, latitude and longitude
-}
sunPosition : Posix -> Coordinated a -> Positioned {}
sunPosition posix coords =
    let
        latitude =
            degrees -coords.longitude

        longitude =
            degrees coords.latitude

        days =
            DaysSince2000.fromPosix posix

        equatorialCoords =
            sunCoords days

        hourAngle =
            siderealTime days latitude - equatorialCoords.rightAscension
    in
    { azimuth = azimuth hourAngle longitude equatorialCoords.declination
    , altitude = altitude hourAngle longitude equatorialCoords.declination
    }


{-| Calculates solar [mean anomaly](https://en.wikipedia.org/wiki/Mean_anomaly) for Earth in radians
-}
earthSolarMeanAnomaly : DaysSince2000 -> Float
earthSolarMeanAnomaly days =
    (earthM1 * unwrap days)
        + earthM0
        |> degrees


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
    (c1 * sin earthSMA)
        + (c2 * sin (2 * earthSMA))
        + (c3 * sin (3 * earthSMA))
        |> degrees


{-| Calculates [ecliptic](https://en.wikipedia.org/wiki/Ecliptic_coordinate_system) longitude for Earth
-}
earthEclipticLongitude : Float -> Float
earthEclipticLongitude earthSMA =
    earthSMA
        + earthEquationOfCenter earthSMA
        + earthPerihelion
        + pi


sunCoords : DaysSince2000 -> EquatorialCoordinated {}
sunCoords days =
    let
        earthSMA =
            earthSolarMeanAnomaly days

        earthEL =
            earthEclipticLongitude earthSMA
    in
    { declination = earthDeclination earthEL sunEclipticLatitude
    , rightAscension = earthRightAscension earthEL sunEclipticLatitude
    }



-- SET/RISE TIMES COMPUTATION FOR SUN


{-| Computes the time where the sun passes at a given angle in the evening.
Example:

    setTime
        -0.833 -- angle
        now    -- current time
        coords -- your coordinates

Use [sunset](#sunset) or [sunsetStart](#sunsetStart) for predefined angles.

-}
setTime : Float -> Posix -> Coordinated a -> Posix
setTime angle posix coords =
    time Set angle posix coords 0


{-| Computes the time where the sun passes at a given angle in the morning.
Example:

    riseTime
        -0.833 -- angle
        now    -- current time
        coords -- your coordinates

Use [sunrise](#sunrise) or [sunriseEnd](#sunsetStart) for predefined angles.

-}
riseTime : Float -> Posix -> Coordinated a -> Posix
riseTime angle posix coords =
    time Rise angle posix coords 0


sunriseSetAngle : Float
sunriseSetAngle =
    -0.833


{-| Computes the time for the sunrise. See also [setTime](#setTime).
-}
sunrise : Posix -> Coordinated a -> Posix
sunrise =
    riseTime sunriseSetAngle


{-| Computes the time for the sunset. See also [riseTime](#riseTime).
-}
sunset : Posix -> Coordinated a -> Posix
sunset =
    setTime sunriseSetAngle


sunriseSetEndAngle : Float
sunriseSetEndAngle =
    -0.3


{-| Computes the time for the end of the sunrise. See also [riseTime](#riseTime).
-}
sunriseEnd : Posix -> Coordinated a -> Posix
sunriseEnd =
    riseTime sunriseSetEndAngle


{-| Computes the time for the begning of the sunset. See also [setTime](#setTime).
-}
sunsetStart : Posix -> Coordinated a -> Posix
sunsetStart =
    setTime sunriseSetEndAngle


type RiseSet
    = Rise
    | Set


{-| Constant needed for time computation (what does it stand for???)
-}
j0 : Float
j0 =
    0.0009


time : RiseSet -> Float -> Posix -> Coordinated a -> Float -> Posix
time riseSet angle posix coords height =
    let
        latitude =
            degrees -coords.longitude

        longitude =
            degrees coords.latitude

        days =
            DaysSince2000.fromPosix posix

        observerAngle =
            -2.076 * sqrt height / 60

        julianCycle =
            days
                |> DaysSince2000.map
                    (\ds ->
                        round (ds - j0 - latitude / (2 * pi))
                            |> toFloat
                    )

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
            acos
                ((sin actualAngle - sin longitude * sin declination)
                    / (cos longitude * cos declination)
                )

        set =
            solarTransit
                (computeApproxTransit hourAngle latitude julianCycle)
                solarMeanAnomaly
                eclipticLongitude
    in
    case riseSet of
        Set ->
            DaysSince2000.toPosix set

        Rise ->
            DaysSince2000.map2
                (\dsSet dsNoon ->
                    2 * dsNoon - dsSet
                )
                set
                noon
                |> DaysSince2000.toPosix


solarTransit : DaysSince2000 -> Float -> Float -> DaysSince2000
solarTransit days solarMeanAnomaly eclipticLongitude =
    days
        |> DaysSince2000.map
            (\ds ->
                ds + 0.0053 * sin solarMeanAnomaly - 0.0069 * sin (2 * eclipticLongitude)
            )


computeApproxTransit : Float -> Float -> DaysSince2000 -> DaysSince2000
computeApproxTransit hourAngle longitude julianCycle =
    julianCycle
        |> DaysSince2000.map
            (\ds ->
                j0 + (hourAngle + longitude) / (2 * pi) + ds
            )



-- MOON COMPUTATIONS


{-| Computes the position of the moon at a given time and position
-}
moonPosition :
    Posix
    -> Coordinated a
    -> Positioned { distance : Float, parallacticAngle : Float }
moonPosition posix coords =
    let
        latitude =
            degrees -coords.longitude

        longitude =
            degrees coords.latitude

        days =
            DaysSince2000.fromPosix posix

        equatorialCoords =
            moonCoords days

        hourAngle =
            siderealTime days latitude - equatorialCoords.rightAscension

        actualAltitude =
            altitude hourAngle longitude equatorialCoords.declination

        -- formula 14.1 of "Astronomical Algorithms" 2nd edition by Jean Meeus (Willmann-Bell, Richmond) 1998.
        parallacticAngle =
            atan2
                (sin hourAngle)
                (tan longitude * cos equatorialCoords.declination - sin equatorialCoords.declination * cos hourAngle)
    in
    { azimuth = azimuth hourAngle longitude equatorialCoords.declination
    , altitude = actualAltitude + astroRefraction actualAltitude
    , distance = equatorialCoords.distance
    , parallacticAngle = parallacticAngle
    }


{-| -}
type alias MoonIllumination =
    { fraction : Float
    , phase : Float
    , angle : Float
    }


{-| Compute moon illumination at a given time
-}

moonIllumination : Posix -> MoonIllumination
moonIllumination = moonIlluminationByDays << DaysSince2000.fromPosix

moonIlluminationByDays : DaysSince2000 -> MoonIllumination
moonIlluminationByDays days =
    let
        sun =
            sunCoords days

        moon =
            moonCoords days

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


moonDay : MoonIllumination -> Int
moonDay illum = 1 + illum.phase * 29 |> floor


type alias EquatorialCoordinated a =
    { a
        | declination : Float
        , rightAscension : Float
    }


moonCoords : DaysSince2000 -> EquatorialCoordinated { distance : Float }
moonCoords days =
    let
        eclipticLongitude =
            (218.316 + 13.176396 * unwrap days) |> degrees

        meanAnomaly =
            (134.963 + 13.064993 * unwrap days) |> degrees

        meanDistance =
            (93.272 + 13.22935 * unwrap days) |> degrees

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


{-| -}
type MoonTimes
    = RiseAt Posix
    | SetAt Posix
    | RiseAndSetAt Posix Posix
    | AlwaysUp
    | AlwaysDown


{-| Computes the times of rise and set (if any) at the day given by
the `Posix` and `Zone` argument.
-}
moonTimes : Time.Zone -> Posix -> Coordinated {} -> MoonTimes
moonTimes zone posix coords =
    let
        midnight =
            Time.Extra.startOfDay zone posix

        initialHeight =
            moonAltitude midnight coords
    in
    loopCrossHorizon 1 midnight coords (CrossResult Nothing Nothing)
        |> toMoonTimes initialHeight midnight


loopCrossHorizon : Int -> Posix -> Coordinated {} -> CrossResult -> CrossResult
loopCrossHorizon offset midnight coords currentResult =
    if offset > 24 then
        currentResult

    else
        let
            newResult =
                crossHorizon (toFloat offset)
                    (moonAltitude (Time.Extra.addHours (offset - 1) midnight) coords)
                    (moonAltitude (Time.Extra.addHours offset midnight) coords)
                    (moonAltitude (Time.Extra.addHours (offset + 1) midnight) coords)
                    currentResult
        in
        if isEnded newResult then
            newResult

        else
            loopCrossHorizon (offset + 2) midnight coords newResult


moonAltitude : Posix -> Coordinated {} -> Float
moonAltitude date coords =
    moonPosition date coords
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
                    { currentResult | riseOffset = Just <| offset + x1 }

                else
                    { currentResult | setOffset = Just <| offset + x1 }

            2 ->
                if ye < 0 then
                    { riseOffset = Just <| offset + x2
                    , setOffset = Just <| offset + x1
                    }

                else
                    { riseOffset = Just <| offset + x1
                    , setOffset = Just <| offset + x2
                    }

            _ ->
                currentResult


isEnded : CrossResult -> Bool
isEnded { riseOffset, setOffset } =
    riseOffset /= Nothing && setOffset /= Nothing


toMoonTimes : Float -> Posix -> CrossResult -> MoonTimes
toMoonTimes height midnight { riseOffset, setOffset } =
    case ( riseOffset, setOffset ) of
        ( Nothing, Nothing ) ->
            if height > 0 then
                AlwaysUp

            else
                AlwaysDown

        ( Nothing, Just s ) ->
            SetAt <|
                addHours s midnight

        ( Just r, Nothing ) ->
            RiseAt <|
                addHours r midnight

        ( Just r, Just s ) ->
            RiseAndSetAt
                (addHours r midnight)
                (addHours s midnight)


addHours : Float -> Posix -> Posix
addHours dt posix =
    posix
        |> Time.posixToMillis
        |> (+) (round <| dt * 3600000)
        |> Time.millisToPosix



-- GENERAL CALCULATIONS FOR POSITION


{-| Calculates [declination](https://en.wikipedia.org/wiki/Declination) for Earth
-}
earthDeclination : Float -> Float -> Float
earthDeclination eclipticLongitude eclipticLatitude =
    (sin eclipticLatitude * cos earthObliquity)
        + (cos eclipticLatitude * sin earthObliquity * sin eclipticLongitude)
        |> asin


{-| Calculates [right ascension](https://en.wikipedia.org/wiki/Right_ascension) for Earth
-}
earthRightAscension : Float -> Float -> Float
earthRightAscension eclipticLongitude eclipticLatitude =
    atan2
        ((sin eclipticLongitude * cos earthObliquity)
            - (tan eclipticLatitude * sin earthObliquity)
        )
        (cos eclipticLongitude)


{-| Calculates [sidereal time](https://en.wikipedia.org/wiki/Sidereal_time) for Earth
-}
siderealTime : DaysSince2000 -> Float -> Float
siderealTime days latitude =
    (earthSiderealTimeJulian2000 + earthSiderealTimeChangeRate * unwrap days)
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
    (sin longitude * sin declination)
        + (cos longitude * cos declination * cos hourAngle)
        |> asin



-- CONSTANTS


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

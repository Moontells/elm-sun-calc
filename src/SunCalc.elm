module SunCalc exposing
    ( Coordinated, Positioned
    , sunPosition
    , MoonIllumination, moonIllumination, moonPosition, riseTime, setTime, sunrise, sunriseEnd, sunset, sunsetStart
    )

{-| This library provides functionality for calculating sun/moon position and light phases.
This is a port of Vladimir Agafonkin's [SunCalc JavaScript library](https://github.com/mourner/suncalc)


# Misc

@docs Coordinated, Positioned


# Sun

@docs sunPosition

-}

-- PUBLIC

import DaysSince2000 exposing (DaysSince2000, unwrap)
import Time exposing (Posix)


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


{-| -}
type alias MoonIllumination =
    { fraction : Float
    , phase : Float
    , angle : Float
    }


{-| Calculates sun position for given time, latitude and longitude
-}
sunPosition : Posix -> Coordinated a -> Result String (Positioned {})
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
    validateCoords coords
        { azimuth = azimuth hourAngle longitude equatorialCoords.declination
        , altitude = altitude hourAngle longitude equatorialCoords.declination
        }


moonPosition :
    Posix
    -> Coordinated a
    -> Result String (Positioned { distance : Float, parallacticAngle : Float })
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
    validateCoords coords
        { azimuth = azimuth hourAngle longitude equatorialCoords.declination
        , altitude = actualAltitude + astroRefraction actualAltitude
        , distance = equatorialCoords.distance
        , parallacticAngle = parallacticAngle
        }


moonIllumination : Posix -> MoonIllumination
moonIllumination posix =
    let
        days =
            DaysSince2000.fromPosix posix

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


{-| Constant needed for computation (what does it stand for???)
-}
j0 : Float
j0 =
    0.0009


type RiseSet
    = Rise
    | Set


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


setTime : Float -> Posix -> Coordinated a -> Result String Posix
setTime angle posix coords =
    validateCoords coords
        (time Set angle posix coords 0)


riseTime : Float -> Posix -> Coordinated a -> Result String Posix
riseTime angle posix coords =
    validateCoords coords
        (time Rise angle posix coords 0)


sunriseSetAngle : Float
sunriseSetAngle =
    -0.833


sunrise : Posix -> Coordinated a -> Result String Posix
sunrise =
    riseTime sunriseSetAngle


sunset : Posix -> Coordinated a -> Result String Posix
sunset =
    setTime sunriseSetAngle


sunriseSetEndAngle : Float
sunriseSetEndAngle =
    -0.3


sunriseEnd : Posix -> Coordinated a -> Result String Posix
sunriseEnd =
    riseTime sunriseSetEndAngle


sunsetStart : Posix -> Coordinated a -> Result String Posix
sunsetStart =
    setTime sunriseSetEndAngle



-- PRIVATE


{-| -}
type alias EquatorialCoordinated a =
    { a
        | declination : Float
        , rightAscension : Float
    }



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



-- GENERAL SUN CALCULATIONS


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



-- GENERAL MOON CALCULATIONS


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



-- VALIDATIONS


{-| Validates coordinates.
Latitude range is between -90° and 90°.
Longitude range is between -180° and 180°.
-}
validateCoords : Coordinated a -> b -> Result String b
validateCoords { latitude, longitude } ok =
    if 90 < latitude || longitude < -90 then
        Err "Latitude is out of range. Latitude should be in range between -90 and 90"

    else if 180 < longitude || longitude < -180 then
        Err "Longitude is out of range"

    else
        Ok ok

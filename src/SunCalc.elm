module SunCalc exposing (sunPosition)

{-| This library provides functionality for calculating sun/moon position and light phases.
This is a port of Vladimir Agafonkin's [SunCalc JavaScript library](https://github.com/mourner/suncalc)


# Sun

@docs sunPosition

-}

-- PUBLIC

import Date exposing (Date)


{-| Calculates sun position for given date, latitude and longitude
-}
sunPosition : Date -> Coordinated a -> Result String (Positioned {})
sunPosition date coords =
    let
        latitude =
            degrees -coords.longitude

        longitude =
            degrees coords.latitude

        days =
            daysSinceJulian2000 date

        equatorialCoords =
            sunCoords days

        hourAngle =
            siderealTime days latitude - equatorialCoords.rightAscension
    in
        validateCoords coords
            { azimuth = azimuth hourAngle longitude equatorialCoords.declination
            , altitude = altitude hourAngle longitude equatorialCoords.declination
            }



-- PRIVATE


type alias Coordinated a =
    { a
        | latitude : Float
        , longitude : Float
    }


type alias EquatorialCoordinated a =
    { a
        | declination : Float
        , rightAscension : Float
    }


type alias Positioned a =
    { a
        | azimuth : Float
        , altitude : Float
    }



-- CONSTANTS


{-| Amount of milliseconds for a day
-}
msPerDay : Float
msPerDay =
    1000 * 60 * 60 * 24


{-| [Julian day number](https://en.wikipedia.org/wiki/Julian_day) for year 1970
-}
julian1970 : Float
julian1970 =
    2440587.5


{-| [Julian day number](https://en.wikipedia.org/wiki/Julian_day) for year 2000
-}
julian2000 : Float
julian2000 =
    2451545


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


{-| First coefficient for calculation of equation of center for Earth
-}
earthC1 : Float
earthC1 =
    1.9148


{-| Second coefficient for calculation of equation of center for Earth
-}
earthC2 : Float
earthC2 =
    0.02


{-| Third coefficient for calculation of equation of center for Earth
-}
earthC3 : Float
earthC3 =
    0.0003


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


{-| [Ecliptic]((https://en.wikipedia.org/wiki/Ecliptic_coordinate_system)) latitude for Sun
-}
sunEclipticLatitude : Float
sunEclipticLatitude =
    0



-- CONVERSIONS


{-| Converts date to [Julian date](https://en.wikipedia.org/wiki/Julian_day)
-}
toJulianDate : Date -> Float
toJulianDate date =
    Date.toTime date / msPerDay + julian1970


{-| Converts [Julian date](https://en.wikipedia.org/wiki/Julian_day) to Gregorian date
-}
fromJulianDate : Float -> Date
fromJulianDate julianDate =
    Date.fromTime <| (julianDate - julian1970) * msPerDay


{-| Calculates amount of days since [Julian day number](https://en.wikipedia.org/wiki/Julian_day) for year 2000
-}
daysSinceJulian2000 : Date -> Float
daysSinceJulian2000 date =
    toJulianDate date - julian2000



-- GENERAL CALCULATIONS FOR POSITION


{-| Calculates [declination](https://en.wikipedia.org/wiki/Declination) for Earth
-}
earthDeclination : Float -> Float -> Float
earthDeclination eclipticLongitude eclipticLatitude =
    asin <| sin eclipticLatitude * cos earthObliquity + cos eclipticLatitude * sin earthObliquity * sin eclipticLongitude


{-| Calculates [right ascension](https://en.wikipedia.org/wiki/Right_ascension) for Earth
-}
eartgRightAscension : Float -> Float -> Float
eartgRightAscension eclipticLongitude eclipticLatitude =
    atan2 (sin eclipticLongitude * cos earthObliquity - tan eclipticLatitude * sin earthObliquity) (cos eclipticLongitude)


{-| Calculates [sidereal time](https://en.wikipedia.org/wiki/Sidereal_time) for Earth
-}
siderealTime : Float -> Float -> Float
siderealTime days latitude =
    degrees (earthSiderealTimeJulian2000 + earthSiderealTimeChangeRate * days) - latitude


{-| Calculates [azimuth](https://en.wikipedia.org/wiki/Azimuth)
-}
azimuth : Float -> Float -> Float -> Float
azimuth hourAngle longitude declination =
    atan2 (sin hourAngle) (cos hourAngle * sin longitude - tan declination * cos longitude)


{-| Calculates [altitude](https://en.wikipedia.org/wiki/Altitude)
-}
altitude : Float -> Float -> Float -> Float
altitude hourAngle longitude declination =
    asin <| sin longitude * sin declination + cos longitude * cos declination * cos hourAngle



-- GENERAL SUN CALCULATIONS


{-| Calculates solar [mean anomaly](https://en.wikipedia.org/wiki/Mean_anomaly) for Earth in radians
-}
earthSolarMeanAnomaly : Float -> Float
earthSolarMeanAnomaly days =
    degrees <| earthM0 + earthM1 * days


{-| Calculates [equitation of center](https://en.wikipedia.org/wiki/Equation_of_the_center) for Earth
-}
earthEquationOfCenter : Float -> Float
earthEquationOfCenter earthSMA =
    degrees <| earthC1 * sin earthSMA + earthC2 * sin (2 * earthSMA) + earthC3 * sin (3 * earthSMA)


{-| Calculates [ecliptic](https://en.wikipedia.org/wiki/Ecliptic_coordinate_system) longitude for Earth
-}
earthEclipticLongitude : Float -> Float
earthEclipticLongitude earthSMA =
    earthSMA + earthEquationOfCenter earthSMA + earthPerihelion + pi


sunCoords : Float -> EquatorialCoordinated {}
sunCoords days =
    let
        earthEL =
            earthEclipticLongitude <| earthSolarMeanAnomaly days
    in
        { declination = earthDeclination earthEL sunEclipticLatitude
        , rightAscension = eartgRightAscension earthEL sunEclipticLatitude
        }



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

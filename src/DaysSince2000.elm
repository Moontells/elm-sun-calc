module DaysSince2000 exposing
    ( DaysSince2000
    , fromPosix
    , map
    , map2
    , toPosix
    , unwrap
    )

import Time


type DaysSince2000
    = DaysSince2000 Float


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



-- CONVERSIONS


{-| Converts date to [Julian date](https://en.wikipedia.org/wiki/Julian_day)
-}
fromPosix : Time.Posix -> DaysSince2000
fromPosix posix =
    toFloat (Time.posixToMillis posix)
        / msPerDay
        + julian1970
        - julian2000
        |> DaysSince2000


{-| Converts [Julian date](https://en.wikipedia.org/wiki/Julian_day) to Gregorian date
-}
toPosix : DaysSince2000 -> Time.Posix
toPosix (DaysSince2000 days) =
    (julian2000 + days - julian1970)
        * msPerDay
        |> round
        |> Time.millisToPosix


unwrap : DaysSince2000 -> Float
unwrap (DaysSince2000 days) =
    days


map : (Float -> Float) -> DaysSince2000 -> DaysSince2000
map f (DaysSince2000 days) =
    DaysSince2000 (f days)


map2 : (Float -> Float -> Float) -> DaysSince2000 -> DaysSince2000 -> DaysSince2000
map2 f (DaysSince2000 days1) (DaysSince2000 days2) =
    DaysSince2000 (f days1 days2)

module Julian exposing
    ( Days
    , Julian
    , daysSince2000
    , fromPosix
    , toPosix
    , unwrap
    )

import Time exposing (Posix)


{-| -}
type Julian
    = Julian Float


type Days
    = Days Float


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
fromPosix : Posix -> Julian
fromPosix posix =
    toFloat (Time.posixToMillis posix)
        / msPerDay
        + julian1970
        |> Julian


{-| Converts [Julian date](https://en.wikipedia.org/wiki/Julian_day) to Gregorian date
-}
toPosix : Julian -> Posix
toPosix (Julian julianDate) =
    (julianDate - julian1970)
        * msPerDay
        |> round
        |> Time.millisToPosix


{-| Calculates amount of days since [Julian day number](https://en.wikipedia.org/wiki/Julian_day) for year 2000
-}
daysSince2000 : Julian -> Days
daysSince2000 (Julian julianDate) =
    Days (julianDate - julian2000)


{-| -}
unwrap : Days -> Float
unwrap (Days days) =
    days

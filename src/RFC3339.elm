module RFC3339 exposing (Date, DateTime, Offset, OffsetDirection(..), Time, dateParser, dateTimeParser, decode, decodeTimestamp, isLeapyear, offsetDirectionParser, offsetParser, paddedIntParser, symbolIgnoreCase, timeParser, toPosix, zOffsetParser, zulu)

{-| This module defines types parsers for a subset of RFC3339
, allows conversion to Time.Posix and implements a Decoder to directly convert RFC3339 Json strings to Time.Posix.
-}

import Array
import Basics exposing (remainderBy)
import Json.Decode as Decode
import Maybe
import Parser exposing ((|.), (|=), Parser, int, map, oneOf, succeed, symbol)
import Time


{-| decode a RFC3339 Json string
-}
decodeTimestamp : Decode.Decoder DateTime
decodeTimestamp =
    let
        fromString : String -> Decode.Decoder DateTime
        fromString s =
            case Parser.run dateTimeParser s of
                Ok dateTime ->
                    Decode.succeed dateTime

                Err _ ->
                    Decode.fail "Failed to parse timestamp"
    in
    Decode.andThen fromString Decode.string


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }



{-
   Parse a YYYY-MM-DD date ("full-date")
-}


dateParser : Parser Date
dateParser =
    succeed Date
        -- year
        |= int
        |. symbol "-"
        -- month
        |= paddedIntParser
        |. symbol "-"
        -- day
        |= paddedIntParser


type alias Time =
    { hours : Int
    , minutes : Int
    , seconds : Int
    }


{-| Parse a HH:MM:SS ("partial time" but without "time-secfrac")
-}
timeParser : Parser Time
timeParser =
    succeed Time
        -- hours
        |= paddedIntParser
        |. symbol ":"
        -- minutes
        |= paddedIntParser
        |. symbol ":"
        -- seconds
        |= paddedIntParser


type alias DateTime =
    { date : Date
    , time : Time
    , offset : Offset
    }


isLeapyear : Int -> Bool
isLeapyear year =
    remainderBy 4 year == 0 && remainderBy 100 year /= 0 || (remainderBy 400 year == 0)


{-| Convert DateTime to Time.Posix. Ported from the C example at <https://de.wikipedia.org/wiki/Unixzeit>
-}
toPosix : DateTime -> Maybe Time.Posix
toPosix { date, time } =
    Maybe.map
        (\daysSinceYearBegin ->
            let
                leapyears =
                    ((date.year - 1) - 1968)
                        // 4
                        - ((date.year - 1) - 1900)
                        // 100
                        + ((date.year - 1) - 1600)
                        // 400

                daysSince1970Base =
                    (date.year - 1970) * 365 + leapyears + daysSinceYearBegin + date.day - 1

                daysSince1970 =
                    if (date.month > 2) && isLeapyear date.year then
                        daysSince1970Base + 1

                    else
                        daysSince1970Base

                seconds =
                    time.seconds + 60 * (time.minutes + 60 * (time.hours + 24 * daysSince1970))
            in
            -- Time.Posix uses milliseconds
            Time.millisToPosix (seconds * 1000)
        )
        (Array.get
            (date.month - 1)
            (Array.fromList [ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ])
        )



{-
   Parse a "date-time" string
-}


dateTimeParser : Parser DateTime
dateTimeParser =
    succeed DateTime
        |= dateParser
        |. symbolIgnoreCase 't'
        |= timeParser
        |= offsetParser


type OffsetDirection
    = Positive
    | Negative


type alias Offset =
    { direction : OffsetDirection
    , hours : Int
    , minutes : Int
    }



{-
   Zulu is the +00:00 UTC offset
-}


zulu : Offset
zulu =
    { direction = Positive, hours = 0, minutes = 0 }


offsetDirectionParser : Parser OffsetDirection
offsetDirectionParser =
    succeed identity
        |= oneOf
            [ map (\_ -> Positive)
                (symbol
                    "+"
                )
            , map
                (\_ -> Negative)
                (symbol
                    "-"
                )
            ]


zOffsetParser : Parser Offset
zOffsetParser =
    map (\_ -> { direction = Positive, hours = 0, minutes = 0 }) (symbolIgnoreCase 'z')


offsetParser : Parser Offset
offsetParser =
    oneOf
        [ succeed Offset
            |= offsetDirectionParser
            |= paddedIntParser
            |. symbol ":"
            |= paddedIntParser
        , zOffsetParser
        ]



{-
   parse integers that may be zero padded (once, like "01", "0123",..)
-}


paddedIntParser : Parser Int
paddedIntParser =
    oneOf
        [ succeed identity
            |. symbol "0"
            |= int
        , succeed identity
            |= int
        ]


{-| Convert a RFC3339 string directly to posix
-}
stringToPosix : String -> Maybe Time.Posix
stringToPosix s =
    s
        |> Parser.run dateTimeParser
        |> Result.toMaybe
        |> Maybe.andThen toPosix


{-| Decode a RFC3339 json string to Time.Posix
-}
decode : Decode.Decoder Time.Posix
decode =
    Decode.andThen
        (\s ->
            case stringToPosix s of
                Just posix ->
                    Decode.succeed posix

                Nothing ->
                    Decode.fail ""
        )
        Decode.string


symbolIgnoreCase : Char -> Parser ()
symbolIgnoreCase char =
    oneOf
        [ symbol (String.fromChar (Char.toUpper char))
        , symbol (String.fromChar (Char.toLower char))
        ]

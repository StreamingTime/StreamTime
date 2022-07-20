module RFC3339 exposing (Date, DateTime, Offset, OffsetDirection(..), Time, dateParser, dateTimeParser, decodeTimestamp, offsetDirectionParser, offsetParser, paddedIntParser, timeParser, zOffsetParser, zulu)

{-| This module defines types parsers for a subset of RFC3339.
-}

import Json.Decode as Decode
import Parser exposing ((|.), (|=), Parser, int, map, oneOf, succeed, symbol)


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



{-
   Parse a "date-time" string
-}


dateTimeParser : Parser DateTime
dateTimeParser =
    succeed DateTime
        |= dateParser
        -- TODO should be case insensitive
        |. symbol "T"
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
    map (\_ -> { direction = Positive, hours = 0, minutes = 0 }) (symbol "Z")


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

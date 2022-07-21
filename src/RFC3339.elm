module RFC3339 exposing (Date, DateTime, Offset, OffsetDirection(..), Time, dateParser, dateTimeParser, decodeTimestamp, format, offsetDirectionParser, offsetParser, paddedIntParser, timeParser, zOffsetParser, zeroPadInt, zulu)

{-| This module defines types parsers for a subset of RFC3339.
-}

import Json.Decode as Decode
import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompUntilEndOr, getChompedString, int, loop, map, oneOf, problem, succeed, symbol, token)


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



-- formatting


zeroPadInt : Int -> Int -> String
zeroPadInt digits i =
    let
        pad d num =
            if String.length num < d then
                "0" ++ pad (d - 1) num

            else
                num
    in
    pad digits (String.fromInt i)


type FormatItem
    = Year
    | Month
    | Day
    | Text String


formatItemToString : FormatItem -> Date -> String
formatItemToString thing { year, month, day } =
    case thing of
        Year ->
            zeroPadInt 2 year

        Month ->
            zeroPadInt 2 month

        Day ->
            zeroPadInt 2 day

        Text text ->
            text


formatItemsToSring : List FormatItem -> Date -> String
formatItemsToSring things date =
    things
        |> List.map (\thing -> formatItemToString thing date)
        |> String.concat


readString : Parser String
readString =
    succeed identity
        |. chompUntilEndOr "%"
        |> getChompedString
        |> andThen
            (\s ->
                if s /= "" then
                    succeed s

                else
                    problem "ende"
            )


parseFormatItem : Parser FormatItem
parseFormatItem =
    succeed identity
        |= oneOf
            [ map (\_ -> Year) (token "%YYYY")
            , map (\_ -> Month) (token "%MM")
            , map (\_ -> Day) (token "%DD")
            , map Text readString
            ]


parseFormatItems : Parser (List FormatItem)
parseFormatItems =
    loop [] parseFormatString


{-| read a format string into a list of FormatItems
-}
parseFormatString : List FormatItem -> Parser (Step (List FormatItem) (List FormatItem))
parseFormatString revStmts =
    -- https://package.elm-lang.org/packages/elm/parser/latest/Parser#loop
    oneOf
        [ succeed
            (\stmt ->
                Loop (stmt :: revStmts)
            )
            |= parseFormatItem
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


{-| Format a Date using the given format string
-}
format : String -> Date -> Result (List Parser.DeadEnd) String
format formatString date =
    Parser.run parseFormatItems formatString
        |> Result.map (\items -> formatItemsToSring items date)

module FormatTime exposing (format, zeroPadInt)

import Parser exposing ((|.), (|=), Parser, Step(..), andThen, chompUntilEndOr, getChompedString, loop, map, oneOf, problem, succeed, token)
import Time



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
    | Hours
    | Minutes
    | Seconds
    | Text String


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


formatItemToString : FormatItem -> Time.Zone -> Time.Posix -> String
formatItemToString thing zone posix =
    case thing of
        Year ->
            zeroPadInt 2 (Time.toYear zone posix)

        Month ->
            zeroPadInt 2 (monthToInt (Time.toMonth zone posix))

        Day ->
            zeroPadInt 2 (Time.toDay zone posix)

        Hours ->
            zeroPadInt 2 (Time.toHour zone posix)

        Minutes ->
            zeroPadInt 2 (Time.toMinute zone posix)

        Seconds ->
            zeroPadInt 2 (Time.toSecond zone posix)

        Text text ->
            text


formatItemsToSring : List FormatItem -> Time.Zone -> Time.Posix -> String
formatItemsToSring things zone posix =
    things
        |> List.map (\thing -> formatItemToString thing zone posix)
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
            [ map (\_ -> Hours) (token "%hh")
            , map (\_ -> Minutes) (token "%mm")
            , map (\_ -> Seconds) (token "%ss")
            , map (\_ -> Year) (token "%YYYY")
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


{-| Format a DateTime using the given format string
-}
format : String -> Time.Zone -> Time.Posix -> Result (List Parser.DeadEnd) String
format formatString zone posix =
    Parser.run parseFormatItems formatString
        |> Result.map (\items -> formatItemsToSring items zone posix)

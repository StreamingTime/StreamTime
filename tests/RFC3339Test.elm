module RFC3339Test exposing (dateParserTest, dateTimeParserTest, decodeTimestampTest, offsetDirectionParserTest, offsetParserTest, paddedIntParserTest, timeParserTest, zOffsetParserTest)

import Expect
import Json.Decode as Decode
import Parser
import RFC3339 exposing (OffsetDirection(..), dateParser, dateTimeParser, decodeTimestamp, offsetDirectionParser, offsetParser, paddedIntParser, timeParser, zOffsetParser, zulu)
import Test exposing (Test, describe, test)


dateParserTest : Test
dateParserTest =
    describe "parse dates"
        [ test "parse date"
            (\_ ->
                let
                    result =
                        Parser.run dateParser "2022-12-21"

                    expected =
                        Result.Ok { year = 2022, month = 12, day = 21 }
                in
                Expect.equal expected result
            )
        , test "parse date with 0 padded numbers"
            (\_ ->
                let
                    result =
                        Parser.run dateParser "2022-02-01"

                    expected =
                        Result.Ok { year = 2022, month = 2, day = 1 }
                in
                Expect.equal expected result
            )
        ]


paddedIntParserTest : Test
paddedIntParserTest =
    describe "parse zero padded integers"
        [ test "parse normal int"
            (\_ ->
                let
                    result =
                        Parser.run paddedIntParser "123"

                    expected =
                        Result.Ok 123
                in
                Expect.equal expected result
            )
        , test "parse 0pad int"
            (\_ ->
                let
                    result =
                        Parser.run paddedIntParser "0123"

                    expected =
                        Result.Ok 123
                in
                Expect.equal expected result
            )
        ]


timeParserTest : Test
timeParserTest =
    describe "parse times"
        [ test "parse time string"
            (\_ ->
                let
                    result =
                        Parser.run timeParser "12:34:56"

                    expected =
                        Result.Ok { hours = 12, minutes = 34, seconds = 56 }
                in
                Expect.equal expected result
            )
        , test "parse 0pad times"
            (\_ ->
                let
                    result =
                        Parser.run timeParser "01:02:03"

                    expected =
                        Result.Ok { hours = 1, minutes = 2, seconds = 3 }
                in
                Expect.equal expected result
            )
        ]


offsetDirectionParserTest : Test
offsetDirectionParserTest =
    describe "parse time zone offset direction"
        [ test "parse plus"
            (\_ ->
                let
                    result =
                        Parser.run offsetDirectionParser "+"

                    expected =
                        Result.Ok Positive
                in
                Expect.equal expected result
            )
        , test "parse minus"
            (\_ ->
                let
                    result =
                        Parser.run offsetDirectionParser "-"

                    expected =
                        Result.Ok Negative
                in
                Expect.equal expected result
            )
        ]


zOffsetParserTest : Test
zOffsetParserTest =
    test "parse Z offset"
        (\_ ->
            let
                result =
                    Parser.run zOffsetParser "Z"

                expected =
                    Result.Ok { direction = Positive, hours = 0, minutes = 0 }
            in
            Expect.equal expected result
        )


offsetParserTest : Test
offsetParserTest =
    describe "parse time zone offset"
        [ test "parse zulu"
            (\_ ->
                Expect.equal (Result.Ok zulu) (Parser.run offsetParser "Z")
            )
        , test "parse positive offset"
            (\_ ->
                let
                    result =
                        Parser.run offsetParser "+12:34"

                    expected =
                        Result.Ok { direction = Positive, hours = 12, minutes = 34 }
                in
                Expect.equal expected result
            )
        , test "parse negative offset"
            (\_ ->
                let
                    result =
                        Parser.run offsetParser "-12:34"

                    expected =
                        Result.Ok { direction = Negative, hours = 12, minutes = 34 }
                in
                Expect.equal expected result
            )
        ]


dateTimeParserTest : Test
dateTimeParserTest =
    describe "parse RFC3339 date time string examples (5.8) without secfracs"
        [ test "a"
            (\_ ->
                let
                    result =
                        Parser.run dateTimeParser "1985-04-12T23:20:50Z"

                    expected =
                        Result.Ok
                            { date =
                                { year = 1985
                                , month = 4
                                , day = 12
                                }
                            , time =
                                { hours = 23
                                , minutes = 20
                                , seconds = 50
                                }
                            , offset = zulu
                            }
                in
                Expect.equal expected result
            )
        , test "b"
            (\_ ->
                let
                    result =
                        Parser.run dateTimeParser "1996-12-19T16:39:57-08:00"

                    expected =
                        Result.Ok
                            { date =
                                { year = 1996
                                , month = 12
                                , day = 19
                                }
                            , time =
                                { hours = 16
                                , minutes = 39
                                , seconds = 57
                                }
                            , offset = { direction = Negative, hours = 8, minutes = 0 }
                            }
                in
                Expect.equal expected result
            )
        , test "c"
            (\_ ->
                let
                    result =
                        Parser.run dateTimeParser "1990-12-31T23:59:60Z"

                    expected =
                        Result.Ok
                            { date =
                                { year = 1990
                                , month = 12
                                , day = 31
                                }
                            , time =
                                { hours = 23
                                , minutes = 59
                                , seconds = 60
                                }
                            , offset = zulu
                            }
                in
                Expect.equal expected result
            )
        , test "d"
            (\_ ->
                let
                    result =
                        Parser.run dateTimeParser "1990-12-31T15:59:60-08:00"

                    expected =
                        Result.Ok
                            { date =
                                { year = 1990
                                , month = 12
                                , day = 31
                                }
                            , time =
                                { hours = 15
                                , minutes = 59
                                , seconds = 60
                                }
                            , offset =
                                { direction = Negative
                                , hours = 8
                                , minutes = 0
                                }
                            }
                in
                Expect.equal expected result
            )
        , test "e"
            (\_ ->
                let
                    result =
                        Parser.run dateTimeParser "1937-01-01T12:00:27+00:20"

                    expected =
                        Result.Ok
                            { date =
                                { year = 1937
                                , month = 1
                                , day = 1
                                }
                            , time =
                                { hours = 12
                                , minutes = 0
                                , seconds = 27
                                }
                            , offset =
                                { direction = Positive
                                , hours = 0
                                , minutes = 20
                                }
                            }
                in
                Expect.equal expected result
            )
        ]


decodeTimestampTest : Test
decodeTimestampTest =
    test "parse json"
        (\_ ->
            let
                expected =
                    Result.Ok
                        { date =
                            { year = 2021
                            , month = 7
                            , day = 1
                            }
                        , time =
                            { hours = 18
                            , minutes = 0
                            , seconds = 0
                            }
                        , offset = zulu
                        }
            in
            "{\"time\": \"2021-07-01T18:00:00Z\"}"
                |> Decode.decodeString (Decode.field "time" decodeTimestamp)
                |> Expect.equal expected
        )

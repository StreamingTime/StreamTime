module RFC3339Test exposing (dateParserTest, dateTimeParserTest, decodeTimestampTest, isLeapyearTest, offsetDirectionParserTest, offsetParserTest, paddedIntParserTest, symbolIgnoreCaseTest, timeParserTest, toPosixTest, zOffsetParserTest)

import Expect
import Json.Decode as Decode
import Parser
import RFC3339 exposing (OffsetDirection(..), dateParser, dateTimeParser, decodeTimestamp, offsetDirectionParser, offsetParser, paddedIntParser, symbolIgnoreCase, timeParser, zOffsetParser, zulu)
import Test exposing (Test, describe, test)
import Time


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
        , test "a but lowercase"
            (\_ ->
                let
                    result =
                        Parser.run dateTimeParser "1985-04-12t23:20:50z"

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


isLeapyearTest : Test
isLeapyearTest =
    describe "correctly determines if a year is a isLeapyearTest"
        [ test "with leapyears"
            (\_ ->
                [ 1904
                , 2040
                , 2140
                , 1944
                , 2044
                , 2144
                , 1948
                , 2048
                , 2148
                , 1952
                , 2052
                , 2068
                , 2168
                , 1972
                , 2072
                , 2172
                , 1976
                , 2076
                , 2176
                , 2092
                , 2192
                , 1996
                , 2096
                , 2196
                , 2096
                , 2196
                , 4000
                , 8000
                ]
                    |> List.all RFC3339.isLeapyear
                    |> Expect.equal True
            )
        , test "with normal years"
            (\_ ->
                [ 2000, 2021, 2022, 2015, 1905, 2195 ]
                    |> List.all RFC3339.isLeapyear
                    |> Expect.equal False
            )
        ]


toPosixTest : Test
toPosixTest =
    describe "convert DateTime to posix"
        [ test "funny number"
            (\_ ->
                RFC3339.toPosix
                    { date =
                        { day = 18
                        , month = 3
                        , year = 2005
                        }
                    , time =
                        { hours = 1
                        , minutes = 58
                        , seconds = 31
                        }
                    , offset = zulu
                    }
                    |> Expect.equal (Just (Time.millisToPosix 1111111111000))
            )
        , test "another funny number"
            (\_ ->
                RFC3339.toPosix
                    { date =
                        { day = 18
                        , month = 5
                        , year = 2033
                        }
                    , time =
                        { hours = 3
                        , minutes = 33
                        , seconds = 20
                        }
                    , offset = zulu
                    }
                    |> Expect.equal (Just (Time.millisToPosix 2000000000000))
            )
        , test "post february leapyear"
            (\_ ->
                RFC3339.toPosix
                    { date =
                        { day = 17
                        , month = 6
                        , year = 2024
                        }
                    , time =
                        { hours = 15
                        , minutes = 0
                        , seconds = 0
                        }
                    , offset = zulu
                    }
                    |> Expect.equal (Just (Time.millisToPosix 1718636400000))
            )
        , test "pre february leapyear"
            (\_ ->
                RFC3339.toPosix
                    { date =
                        { day = 11
                        , month = 1
                        , year = 2024
                        }
                    , time =
                        { hours = 15
                        , minutes = 37
                        , seconds = 19
                        }
                    , offset = zulu
                    }
                    |> Expect.equal (Just (Time.millisToPosix 1704987439000))
            )
        ]


symbolIgnoreCaseTest : Test
symbolIgnoreCaseTest =
    describe "test case insensitive symbol parsing"
        [ test "uppercase char"
            (\_ ->
                Parser.run (symbolIgnoreCase 'a') "A"
                    |> Expect.ok
            )
        , test
            "lowercase char"
            (\_ ->
                Parser.run (symbolIgnoreCase 'a') "A"
                    |> Expect.ok
            )
        , test
            "totally different char schould fail"
            (\_ ->
                Parser.run (symbolIgnoreCase 'b') "A"
                    |> Expect.err
            )
        ]

module FormatTimeTest exposing (formatTest, zeroPadIntTest)

import Expect
import FormatTime exposing (format, zeroPadInt)
import RFC3339 exposing (zulu)
import Test exposing (Test, describe, test)
import Time


formatTest : Test
formatTest =
    test "format date"
        (\_ ->
            case
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
            of
                Just posix ->
                    format "%DD.%MM.%YYYY" Time.utc posix
                        |> Expect.equal (Ok "18.03.2005")

                Nothing ->
                    Expect.fail "failed to get posix time"
        )


zeroPadIntTest : Test
zeroPadIntTest =
    describe "0 pad ints"
        [ test "pad once"
            (\_ ->
                zeroPadInt 2 1
                    |> Expect.equal "01"
            )
        , test "pad to three digits"
            (\_ ->
                zeroPadInt 3 1
                    |> Expect.equal "001"
            )
        , test "no padding needed"
            (\_ ->
                zeroPadInt 4 1234
                    |> Expect.equal "1234"
            )
        ]

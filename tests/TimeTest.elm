module TimeTest exposing (sameDayTest, timeInDaysTest, toWeekdayStringTest)

import Expect
import Test exposing (Test, describe, test)
import Time
import Time.Extra


toWeekdayStringTest : Test
toWeekdayStringTest =
    describe "to weekday string test"
        [ test "monday"
            (\_ ->
                Time.Extra.toWeekdayString Time.utc (Time.millisToPosix 1659948899519)
                    |> Expect.equal "Mon"
            )
        , test "tuesday"
            (\_ ->
                Time.Extra.toWeekdayString Time.utc (Time.millisToPosix 1660035299519)
                    |> Expect.equal "Tue"
            )
        , test "wednesday"
            (\_ ->
                Time.Extra.toWeekdayString Time.utc (Time.millisToPosix 1660121699519)
                    |> Expect.equal "Wed"
            )
        , test "thursday"
            (\_ ->
                Time.Extra.toWeekdayString Time.utc (Time.millisToPosix 1660208099519)
                    |> Expect.equal "Thu"
            )
        , test "friday"
            (\_ ->
                Time.Extra.toWeekdayString Time.utc (Time.millisToPosix 1660294499519)
                    |> Expect.equal "Fri"
            )
        , test "saturday"
            (\_ ->
                Time.Extra.toWeekdayString Time.utc (Time.millisToPosix 1660380899519)
                    |> Expect.equal "Sat"
            )
        , test "sunday"
            (\_ ->
                Time.Extra.toWeekdayString Time.utc (Time.millisToPosix 1660467299519)
                    |> Expect.equal "Sun"
            )
        ]


timeInDaysTest : Test
timeInDaysTest =
    test "time in days test"
        (\_ ->
            Time.Extra.timeInDays (Time.millisToPosix 0) 1
                |> Expect.equal (Time.millisToPosix 86400000)
        )


sameDayTest : Test
sameDayTest =
    describe "same day test"
        [ test "same day"
            (\_ ->
                Time.Extra.sameDay Time.utc (Time.millisToPosix 0) (Time.millisToPosix 82800000)
                    |> Expect.true "Expected to be the same day"
            )
        , test "not same day"
            (\_ ->
                Time.Extra.sameDay Time.utc (Time.millisToPosix 0) (Time.millisToPosix 90000000)
                    |> Expect.false "Expected not to be the same day"
            )
        ]

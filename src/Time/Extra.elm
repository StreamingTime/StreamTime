module Time.Extra exposing (sameDay, timeInDays, timeInOneWeek, toWeekdayString)

import Time


toWeekdayString : Time.Zone -> Time.Posix -> String
toWeekdayString zone time =
    case Time.toWeekday zone time of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


timeInDays : Time.Posix -> Int -> Time.Posix
timeInDays time days =
    time
        |> Time.posixToMillis
        -- 60 seconds * 60 minutes * 24 hours * X days (in ms)
        |> (+) (60 * 60 * 24 * days * 1000)
        |> Time.millisToPosix


timeInOneWeek : Time.Posix -> Time.Posix
timeInOneWeek time =
    timeInDays time 7


sameDay : Time.Zone -> Time.Posix -> Time.Posix -> Bool
sameDay timeZone x y =
    Time.toDay timeZone x
        == Time.toDay timeZone y
        && Time.toMonth timeZone x
        == Time.toMonth timeZone y
        && Time.toYear timeZone x
        == Time.toYear timeZone y

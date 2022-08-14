module Views.Calendar exposing (calendarView)

import Error exposing (Error)
import FormatTime
import Html.Styled exposing (Html, div, p, text)
import Html.Styled.Attributes exposing (css, style)
import Http
import List.Extra
import RFC3339
import RefreshData exposing (RefreshData)
import Tailwind.Utilities as Tw
import Time
import Time.Extra
import Twitch
import Utils exposing (findUserByID)
import Views.ScheduleSegment exposing (scheduleSegmentView)


calendarView : Time.Zone -> Time.Posix -> RefreshData Error (List Twitch.User) -> RefreshData Error (List Twitch.Schedule) -> List Twitch.User -> Html msg
calendarView timezone time streamers schedules selected =
    div
        [ css
            [ Tw.rounded_md
            , Tw.bg_dark_500
            , Tw.py_4
            , Tw.px_2
            , Tw.overflow_x_auto
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.justify_center
                , Tw.space_y_4
                ]
            ]
            (div [ css [ Tw.flex ] ]
                [ div
                    [ style "flex-shrink" "0"
                    , css
                        [ Tw.flex
                        , Tw.h_10
                        , Tw.w_20
                        , Tw.flex_col
                        , Tw.justify_center
                        , Tw.text_center
                        , Tw.text_sm
                        ]
                    ]
                    [ p [] [ text "GMT" ]
                    , p [] [ text (FormatTime.format "%TZ" timezone time |> Result.withDefault "") ]
                    ]
                , div
                    [ css
                        [ Tw.flex
                        , Tw.w_full
                        , Tw.flex_col
                        , Tw.justify_center
                        , Tw.text_lg
                        , Tw.font_semibold
                        ]
                    ]
                    [ div [ css [ Tw.grid, Tw.grid_cols_calendar ] ]
                        [ p [] [ text "00:00" ]
                        , p [] [ text "02:00" ]
                        , p [] [ text "04:00" ]
                        , p [] [ text "06:00" ]
                        , p [] [ text "08:00" ]
                        , p [] [ text "10:00" ]
                        , p [] [ text "12:00" ]
                        , p [] [ text "14:00" ]
                        , p [] [ text "16:00" ]
                        , p [] [ text "18:00" ]
                        , p [] [ text "20:00" ]
                        , p [] [ text "22:00" ]
                        ]
                    ]
                ]
                :: dayViews timezone time streamers schedules selected
            )
        ]


dayViews : Time.Zone -> Time.Posix -> RefreshData Error (List Twitch.User) -> RefreshData Error (List Twitch.Schedule) -> List Twitch.User -> List (Html msg)
dayViews timeZone time streamers schedules selected =
    let
        weekDayString t =
            Time.Extra.toWeekdayString timeZone t

        dateString t =
            case FormatTime.format "%DD.%MM." timeZone t of
                Ok value ->
                    value

                Err _ ->
                    "00.00."

        views offsetDays =
            if offsetDays >= 7 then
                []

            else
                div [ css [ Tw.flex ] ]
                    [ div
                        [ style "flex-shrink" "0"
                        , css
                            [ Tw.flex
                            , Tw.flex_col
                            , Tw.w_20
                            , Tw.items_center
                            , Tw.pt_4
                            , Tw.font_semibold
                            ]
                        ]
                        [ p [] [ Time.Extra.timeInDays time offsetDays |> weekDayString |> text ]
                        , p [] [ Time.Extra.timeInDays time offsetDays |> dateString |> text ]
                        ]
                    , div
                        [ css
                            [ Tw.grid
                            , Tw.grid_cols_calendar_mark
                            , Tw.w_full
                            , Tw.space_y_4
                            ]
                        ]
                        (div [] []
                            :: schedulesViews
                                timeZone
                                (Time.Extra.timeInDays time offsetDays)
                                (RefreshData.mapTo (\_ -> identity) streamers)
                                (RefreshData.mapTo (\_ -> identity) schedules
                                    |> Utils.schedulesWithStreamers selected
                                )
                        )
                    ]
                    :: views (offsetDays + 1)
    in
    views 0


schedulesViews : Time.Zone -> Time.Posix -> List Twitch.User -> List Twitch.Schedule -> List (Html msg)
schedulesViews timeZone time streamers schedules =
    let
        filterSegments =
            List.filter
                (\segment ->
                    case RFC3339.toPosix segment.startTime of
                        Just value ->
                            Time.Extra.sameDay timeZone time value

                        Nothing ->
                            False
                )
    in
    schedules
        |> List.map (\schedule -> ( schedule.broadcasterId, schedule.segments ))
        |> List.Extra.filterIndexedMap
            (\index ( userID, segments ) ->
                findUserByID userID streamers
                    |> Maybe.map
                        (\user ->
                            List.concat
                                [ filterSegments segments
                                    |> List.map (scheduleTimeSegment timeZone (index + 1))
                                , filterSegments segments
                                    |> List.map (scheduleContentSegment timeZone user (index + 1))
                                ]
                        )
            )
        |> List.concat


scheduleTimeSegment : Time.Zone -> Int -> Twitch.Segment -> Html msg
scheduleTimeSegment timeZone row segment =
    let
        startTimePosix =
            RFC3339.toPosix segment.startTime

        startMinutes =
            case startTimePosix of
                Just value ->
                    Time.toHour timeZone value * 60 + Time.toMinute timeZone value

                Nothing ->
                    0

        endTimePosix =
            RFC3339.toPosix segment.endTime

        endMinutes =
            case ( startTimePosix, endTimePosix ) of
                ( Just s, Just e ) ->
                    {- if it is not the same day we set endMinutes to the end of the current day:
                       24 * 60 minutes = 1440 minutes
                    -}
                    if Time.Extra.sameDay timeZone s e then
                        Time.toHour timeZone e * 60 + Time.toMinute timeZone e

                    else
                        1440

                _ ->
                    0

        {- One time segment represents 30 minutes. Therefore we divide by 30 and add 1,
           because grid numeration starts with 1.
        -}
        start =
            startMinutes // 30 + 1

        end =
            endMinutes // 30 + 1
    in
    div
        [ css
            [ Tw.rounded_sm
            , Tw.bg_purple_700
            ]
        , style "grid-row-start" (String.fromInt row)
        , style "grid-column" (String.concat [ String.fromInt start, " / ", String.fromInt end ])
        ]
        []


scheduleContentSegment : Time.Zone -> Twitch.User -> Int -> Twitch.Segment -> Html msg
scheduleContentSegment timeZone user row segment =
    div
        [ css
            [ Tw.col_span_full
            , Tw.py_1
            ]
        , style "grid-row-start" (String.fromInt row)
        ]
        [ scheduleSegmentView timeZone user segment ]

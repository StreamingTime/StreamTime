module Views.Calendar exposing (calendarView)

import Error exposing (Error(..))
import FormatTime
import Html.Styled exposing (Html, div, text)
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


calendarView : Time.Zone -> Time.Posix -> RefreshData Http.Error (List Twitch.User) -> RefreshData Error (List Twitch.Schedule) -> Html msg
calendarView timezone time streamers schedules =
    div
        [ css
            [ Tw.rounded_md
            , Tw.bg_dark_500
            , Tw.p_2
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
            ([ -- Header
               div [ css [ Tw.flex ] ]
                [ div
                    [ css
                        [ Tw.flex
                        , Tw.h_10
                        , Tw.w_24
                        , Tw.flex_col
                        , Tw.justify_center
                        , Tw.text_center
                        ]
                    ]
                    [ text "GMT +2" ]
                , div
                    [ css
                        [ Tw.flex
                        , Tw.w_full
                        , Tw.flex_col
                        , Tw.justify_center
                        ]
                    ]
                    [ div [ css [ Tw.grid, Tw.grid_cols_12 ] ]
                        [ div [] [ text "00:00" ]
                        , div [] [ text "02:00" ]
                        , div [] [ text "04:00" ]
                        , div [] [ text "06:00" ]
                        , div [] [ text "08:00" ]
                        , div [] [ text "10:00" ]
                        , div [] [ text "12:00" ]
                        , div [] [ text "14:00" ]
                        , div [] [ text "16:00" ]
                        , div [] [ text "18:00" ]
                        , div [] [ text "20:00" ]
                        , div [] [ text "22:00" ]
                        ]
                    ]
                ]

             -- Table View
             ]
                ++ dayViews timezone time streamers schedules
            )
        ]


dayViews : Time.Zone -> Time.Posix -> RefreshData Http.Error (List Twitch.User) -> RefreshData Error (List Twitch.Schedule) -> List (Html msg)
dayViews timeZone time streamers schedules =
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
                        [ css
                            [ Tw.flex
                            , Tw.flex_col
                            , Tw.w_24
                            , Tw.items_center
                            , Tw.pt_4
                            ]
                        ]
                        [ div [] [ Time.Extra.timeInDays time offsetDays |> weekDayString |> text ]
                        , div [] [ Time.Extra.timeInDays time offsetDays |> dateString |> text ]
                        ]
                    , div
                        [ css
                            [ Tw.grid
                            , Tw.grid_cols_48
                            , Tw.w_full
                            , Tw.space_y_4
                            ]
                        ]
                        (div [] []
                            :: schedulesViews
                                timeZone
                                (Time.Extra.timeInDays time offsetDays)
                                (RefreshData.mapTo (\_ -> identity) streamers)
                                (RefreshData.mapTo (\_ -> identity) schedules)
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
                                    |> List.map (\_ -> scheduleTimeSegment (index + 1))
                                , filterSegments segments
                                    |> List.map (scheduleContentSegment timeZone user (index + 1))
                                ]
                        )
            )
        |> List.concat


scheduleTimeSegment : Int -> Html msg
scheduleTimeSegment row =
    let
        -- TODO
        start =
            3

        end =
            20
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
            , Tw.py_2
            ]
        , style "grid-row-start" (String.fromInt row)
        ]
        [ scheduleSegmentView timeZone user segment ]

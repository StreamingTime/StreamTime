module ScheduleSegment exposing (scheduleSegmentView)

import FormatTime
import Html.Styled exposing (Html, div, p, span, text)
import Html.Styled.Attributes exposing (css)
import RFC3339
import Tailwind.Utilities as Tw
import Time
import Twitch


scheduleSegmentView : Time.Zone -> Twitch.Segment -> Html msg
scheduleSegmentView zone { title, startTime, endTime, category } =
    let
        categoryView =
            case category of
                Just c ->
                    p [] [ text c.name ]

                Nothing ->
                    text ""

        date format dt =
            let
                posix =
                    case RFC3339.toPosix dt of
                        Just p ->
                            p

                        Nothing ->
                            Time.millisToPosix 0
            in
            case FormatTime.format format zone posix of
                Ok s ->
                    s

                Err _ ->
                    "Failed to parse date"
    in
    div [ css [ Tw.rounded, Tw.bg_gray_600 ] ]
        [ p [] [ text title ]
        , categoryView
        , p []
            [ span [] [ text (date "%DD.%MM.%YYYY %hh:%mm " startTime) ]
            , span [] [ text " - " ]
            , span [] [ text (date "%DD.%MM.%YYYY %hh:%mm" endTime) ]
            ]
        ]

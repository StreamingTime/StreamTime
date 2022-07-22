module ScheduleSegment exposing (scheduleSegmentView)

import FormatTime
import Html.Styled exposing (Html, button, div, p, span, text)
import Html.Styled.Attributes exposing (attribute, class, css)
import Icons
import RFC3339
import Tailwind.Utilities as Tw
import Time
import Twitch


scheduleSegmentView : Time.Zone -> Twitch.Segment -> Html msg
scheduleSegmentView zone { title, startTime, endTime, category, isRecurring } =
    let
        categoryView =
            case category of
                Just c ->
                    p [ css [ Tw.text_primary ] ] [ text c.name ]

                Nothing ->
                    text ""

        asPosix dt =
            case RFC3339.toPosix dt of
                Just p ->
                    p

                Nothing ->
                    Time.millisToPosix 0

        date format dt =
            case FormatTime.format format zone (asPosix dt) of
                Ok s ->
                    s

                Err _ ->
                    "Failed to parse date"

        titleView =
            div []
                [ if String.isEmpty title then
                    p [ css [ Tw.italic ] ] [ text "Untitled stream" ]

                  else
                    p [] [ text title ]
                ]

        endTimeFormat =
            if Time.toDay zone (asPosix startTime) == Time.toDay zone (asPosix endTime) then
                "%hh:%mm GMT%TZ"

            else
                "%DD.%MM.%YYYY %hh:%mm GMT%TZ"

        isRecurringView =
            if isRecurring then
                div
                    [ attribute "data-tip" "Repeats every week"
                    , class "tooltip"
                    , css [ Tw.tooltip, Tw.tooltip_primary, Tw.ml_1 ]
                    ]
                    [ button []
                        [ Icons.repeat
                            16
                            16
                        ]
                    ]

            else
                text ""

        datesView =
            div [ css [ Tw.inline ] ]
                [ span [] [ text (date "%DD.%MM.%YYYY %hh:%mm" startTime) ]
                , span [] [ text " - " ]
                , span [] [ text (date endTimeFormat endTime) ]
                ]
    in
    div [ css [ Tw.rounded, Tw.bg_base_300 ] ]
        [ titleView
        , categoryView
        , datesView
        , isRecurringView
        ]

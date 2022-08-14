module Views.ScheduleSegment exposing (scheduleSegmentView)

import FormatTime
import Html.Styled exposing (Html, div, img, p, span, text)
import Html.Styled.Attributes exposing (alt, attribute, class, css, height, src, width)
import Icons
import RFC3339
import Tailwind.Utilities as Tw
import Time
import Twitch


scheduleSegmentView : Time.Zone -> Twitch.User -> Twitch.Segment -> Html msg
scheduleSegmentView zone { displayName, profileImageUrl } { title, startTime, endTime, category, isRecurring, canceledUntil } =
    let
        strikeIfCanceled =
            case canceledUntil of
                Just _ ->
                    css [ Tw.line_through ]

                Nothing ->
                    css []

        categoryView =
            case category of
                Just c ->
                    p [ css [ Tw.text_primary ], strikeIfCanceled ]
                        [ text c.name ]

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
            div [ strikeIfCanceled ]
                [ if String.isEmpty title then
                    div [ css [ Tw.italic ] ] [ text "Untitled stream" ]

                  else
                    p [] [ text title ]
                ]

        isRecurringView =
            if isRecurring then
                div
                    [ attribute "data-tip" "Repeats every week"
                    , class "tooltip"
                    , css [ Tw.tooltip, Tw.tooltip_primary, Tw.ml_1 ]
                    ]
                    [ div []
                        [ Icons.repeat
                            [ Tw.icon_neutral, Tw.icon_m ]
                        ]
                    ]

            else
                text ""

        datesView =
            div [ strikeIfCanceled ]
                (span [] [ text (date "%DD.%MM.%YYYY %hh:%mm" startTime) ]
                    :: (case endTime of
                            Just value ->
                                let
                                    endTimeFormat =
                                        if Time.toDay zone (asPosix startTime) == Time.toDay zone (asPosix value) then
                                            "%hh:%mm GMT%TZ"

                                        else
                                            "%DD.%MM.%YYYY %hh:%mm GMT%TZ"
                                in
                                [ span [] [ text " - " ]
                                , span [] [ text (date endTimeFormat value) ]
                                , isRecurringView
                                ]

                            Nothing ->
                                [ isRecurringView ]
                       )
                )

        categoryImageView =
            case category of
                Just c ->
                    img
                        [ css [ Tw.rounded_r ]
                        , width 57
                        , height 76
                        , src (Twitch.boxArtUrl c 144 192)
                        , alt c.name
                        ]
                        []

                Nothing ->
                    text ""

        canceledInfoView =
            case canceledUntil of
                Just untilDate ->
                    div
                        [ attribute "data-tip" ("Canceled until " ++ date "%DD.%MM.%YYYY %hh:%mm" untilDate)
                        , class "tooltip"
                        , css [ Tw.m_1, Tw.tooltip, Tw.tooltip_primary ]
                        ]
                        [ Icons.warning [ Tw.icon_warning, Tw.icon_m ] ]

                Nothing ->
                    text ""

        avatar =
            div [ css [ Tw.avatar, Tw.flex, Tw.items_center ] ]
                [ div
                    [ css
                        [ Tw.rounded_full
                        , Tw.w_10
                        , Tw.h_10
                        , Tw.m_2
                        ]
                    ]
                    [ img
                        [ src profileImageUrl
                        , alt displayName
                        , css
                            [ Tw.flex
                            , Tw.content_center
                            ]
                        ]
                        []
                    ]
                ]
    in
    div
        [ css [ Tw.rounded, Tw.bg_dark_800, Tw.flex, Tw.justify_between ]
        ]
        [ avatar
        , div [ css [ Tw.flex, Tw.justify_center, Tw.flex_col, Tw.flex_grow ] ]
            [ titleView
            , div [ css [ Tw.flex ] ]
                [ canceledInfoView
                , categoryView
                ]
            , datesView
            ]
        , categoryImageView
        ]

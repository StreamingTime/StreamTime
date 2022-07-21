module ScheduleSegment exposing (scheduleSegmentView)

import Html.Styled exposing (Html, div, p, span, text)
import Html.Styled.Attributes exposing (css)
import RFC3339
import Tailwind.Utilities as Tw
import Twitch


scheduleSegmentView : Twitch.Segment -> Html msg
scheduleSegmentView { title, startTime, category } =
    let
        categoryView =
            case category of
                Just c ->
                    p [] [ text c.name ]

                Nothing ->
                    text ""

        date =
            case RFC3339.format "%DD.%MM.%YYYY" startTime.date of
                Ok s ->
                    s

                Err _ ->
                    "Failed to parse date"
    in
    div [ css [ Tw.rounded, Tw.bg_gray_600 ] ]
        [ p [] [ text title ]
        , categoryView
        , p []
            [ span [] [ text date ]
            , span [] [ text " • " ]
            ]
        ]

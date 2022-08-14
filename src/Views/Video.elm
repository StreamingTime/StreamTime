module Views.Video exposing (videoListView)

import Dict
import FormatTime
import Html.Styled exposing (Html, a, div, img, p, text)
import Html.Styled.Attributes exposing (css, height, href, src, width)
import List.Extra
import Tailwind.Utilities as Tw
import Time
import Twitch exposing (VideoType(..))


dayString : Time.Posix -> String
dayString posix =
    posix
        |> FormatTime.format "%Month %DD, %YYYY" Time.utc
        |> Result.withDefault "Failed to format date"


videoListView : List Twitch.Video -> Html msg
videoListView videos =
    div
        [ css [ Tw.m_5 ] ]
        (videos
            -- Time.Posix is not comparable, so we build the date heading here and use it as key to group the videos
            |> List.Extra.groupBy (\video -> dayString video.createdAt)
            |> Dict.map dayView
            |> Dict.toList
            |> List.map Tuple.second
        )


dayView : String -> List Twitch.Video -> Html msg
dayView dayText videos =
    div
        []
        [ p
            [ css [ Tw.font_bold ]
            , css [ Tw.mt_5, Tw.mb_5 ]
            ]
            [ text dayText ]
        , div
            [ css [ Tw.grid, Tw.grid_cols_3, Tw.gap_5 ] ]
            (List.map videoView videos)
        ]


videoView : Twitch.Video -> Html msg
videoView { title, thumbnailURL, videoType, duration, userName, url } =
    let
        bottomRight =
            css
                [ Tw.absolute
                , Tw.right_1
                , Tw.bottom_1
                ]

        topLeft =
            css
                [ Tw.absolute
                , Tw.left_1
                , Tw.top_1
                ]

        thumbnailView =
            div [ css [ Tw.relative, Tw.inline_block ] ]
                [ img
                    [ src (Twitch.videoPreview 290 164 thumbnailURL)
                    , width 290
                    , height 164
                    ]
                    []
                , div
                    [ bottomRight
                    , css
                        [ Tw.rounded
                        , Tw.bg_black
                        , Tw.text_sm
                        ]
                    ]
                    [ text duration ]
                , div [ topLeft ]
                    [ typeView ]
                ]

        typeView =
            p [ css [ Tw.badge ] ]
                [ text
                    (case videoType of
                        Upload ->
                            "Upload"

                        Archive ->
                            "Archive"

                        Highlight ->
                            "Highlight"
                    )
                ]

        streamerNameView =
            a
                [ css [ Tw.text_primary ]
                , href (Twitch.userProfileUrl userName)
                ]
                [ p [] [ text userName ] ]
    in
    div
        [ css
            [ Tw.p_5
            , Tw.rounded
            , Tw.inline_block
            , Tw.bg_dark_800
            ]
        ]
        [ a [ href url ] [ thumbnailView ]
        , streamerNameView
        , p [] [ text title ]
        ]

module Views.Video exposing (videoView)

import Html.Styled exposing (Html, a, div, img, p, text)
import Html.Styled.Attributes exposing (css, height, href, src, width)
import Tailwind.Utilities as Tw
import Twitch exposing (VideoType(..))


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
            [ Tw.m_10
            , Tw.p_5
            , Tw.rounded
            , Tw.inline_block
            , Tw.bg_dark_800
            ]
        ]
        [ a [ href url ] [ thumbnailView ]
        , streamerNameView
        , p [] [ text title ]
        ]

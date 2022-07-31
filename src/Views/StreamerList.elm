module Views.StreamerList exposing (StreamerListMsg(..), streamerListPageSteps, streamerListView)

import Components exposing (loadingSpinner)
import Css
import Error
import Html.Styled exposing (Html, a, button, div, hr, img, input, label, p, span, text)
import Html.Styled.Attributes exposing (css, href, placeholder, src, type_)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import RefreshData exposing (RefreshData)
import Tailwind.Utilities as Tw
import Twitch
import Utils exposing (filterFollowsByLogin)


type StreamerListMsg
    = ShowMore
    | ShowLess
    | Filter String
    | SetStreamerSelection Twitch.User Bool


streamerListPageSteps : Int
streamerListPageSteps =
    10


streamerListView : RefreshData Http.Error (List ( Twitch.User, Bool )) -> List Twitch.FollowRelation -> Int -> Maybe String -> Html StreamerListMsg
streamerListView streamersData follows showCount filterString =
    let
        streamers =
            RefreshData.mapTo (\_ list -> list) streamersData

        selectedStreamers =
            streamers
                |> List.filter (\( _, selected ) -> selected)

        unselectedStreamers =
            streamers
                |> List.filter (\( _, selected ) -> not selected)

        restStreamersView =
            div []
                (unselectedStreamers
                    |> List.take showCount
                    |> List.map (\( streamer, isSelected ) -> streamerView streamer isSelected)
                )

        linkButtonStyle =
            css [ Tw.text_primary, Tw.underline ]

        moreAvailable =
            List.length unselectedStreamers > showCount || List.length follows > List.length streamers

        buttons =
            div
                [ css
                    ([ Tw.mt_2
                     , Tw.mx_2
                     , Tw.flex
                     , Tw.justify_between
                     ]
                        ++ (if not moreAvailable then
                                [ Tw.flex_row_reverse ]

                            else
                                []
                           )
                    )
                ]
                [ if moreAvailable then
                    button [ linkButtonStyle, onClick ShowMore ] [ text "Show more" ]

                  else
                    text ""
                , if showCount > streamerListPageSteps then
                    button [ linkButtonStyle, onClick ShowLess ] [ text "Show less" ]

                  else
                    text ""
                ]

        spinner =
            if RefreshData.isLoading streamersData then
                loadingSpinner
                    [ Tw.w_8
                    , Tw.h_8
                    , Tw.mt_2
                    , Tw.mx_2
                    ]

            else
                text ""

        errorText =
            RefreshData.mapTo
                (\err _ ->
                    case err of
                        Just error ->
                            div [ css [ Tw.mt_2, Tw.mx_2 ] ] [ text (Error.httpErrorToString error) ]

                        Nothing ->
                            text ""
                )
                streamersData

        filterUI =
            div [ css [ Tw.form_control ] ]
                [ label
                    [ css [ Tw.label ]
                    ]
                    [ span [ css [ Tw.label_text ] ] [ text "Search" ]
                    ]
                , input
                    [ type_ "text"
                    , placeholder "Channel name"
                    , css [ Tw.input, Tw.input_ghost, Tw.input_bordered, Tw.input_sm, Tw.m_1 ]
                    , onInput (\s -> Filter s)
                    ]
                    []
                ]

        filteredList =
            case filterString of
                Just query ->
                    if String.length query >= 4 then
                        -- this is also computed in update, so we could save us the computation here
                        (filterFollowsByLogin query follows
                            |> List.map
                                (\follow ->
                                    let
                                        streamerProfile =
                                            streamers
                                                |> List.filter (\( user, _ ) -> user.id == follow.toID)
                                                |> List.head
                                    in
                                    case streamerProfile of
                                        Just ( isSelected, streamer ) ->
                                            streamerView isSelected streamer

                                        Nothing ->
                                            loadingSpinner [ Tw.w_8, Tw.h_8 ]
                                )
                        )
                            ++ [ hr [] [] ]

                    else if String.length query > 0 then
                        [ text "Enter at least 4 characters" ]

                    else
                        [ text "" ]

                Nothing ->
                    [ text "" ]

        filterResultsView =
            if List.isEmpty filteredList then
                text ""

            else
                div []
                    filteredList

        selectedView =
            if List.isEmpty selectedStreamers then
                text ""

            else
                div []
                    ((selectedStreamers
                        |> List.map (\( selected, streamer ) -> streamerView selected streamer)
                     )
                        ++ [ hr [] [] ]
                    )
    in
    div
        [ css
            [ Tw.bg_base_200
            , Tw.fixed
            , Tw.top_16
            , Tw.bottom_0
            , Tw.w_60
            , Tw.overflow_y_auto

            -- hide scrollbar in firefox browsers
            , Css.property "scrollbar-width" "none"

            -- hide scrollbar in chrome, edge, opera and other browsers
            , Css.pseudoClass ":-webkit-scrollbar" [ Css.width (Css.px 0) ]
            ]
        ]
        [ div
            [ css
                [ Tw.my_2
                , Tw.text_sm
                , Tw.font_medium
                ]
            ]
            [ p [ css [ Tw.text_center ] ] [ text "CHANNELS YOU FOLLOW" ]
            , div [ css [ Tw.mt_2 ] ]
                [ filterUI
                , div
                    []
                    [ filterResultsView, selectedView, restStreamersView, buttons, errorText, spinner ]
                ]
            ]
        ]


streamerView : Twitch.User -> Bool -> Html StreamerListMsg
streamerView streamer isSelected =
    let
        avatar =
            div [ css [ Tw.avatar ] ]
                [ div
                    [ css
                        [ Tw.rounded_full
                        , Tw.w_10
                        , Tw.h_10
                        , Css.hover [ Tw.ring, Tw.ring_primary_focus ]
                        ]
                    ]
                    [ img [ src streamer.profileImageUrl ] []
                    ]
                ]
    in
    div
        [ css
            [ Tw.block
            , Tw.p_1
            , Css.hover [ Tw.bg_purple_500 ]
            , Tw.cursor_pointer
            ]
        , onClick
            (SetStreamerSelection streamer (not isSelected))
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.space_x_2
                , Tw.items_center
                ]
            ]
            [ a [ href ("https://twitch.tv/" ++ streamer.displayName) ] [ avatar ]
            , div [ css [ Tw.font_medium, Tw.truncate ] ] [ text streamer.displayName ]
            , text
                (if isSelected then
                    "✅"

                 else
                    ""
                )
            ]
        ]

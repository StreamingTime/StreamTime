module Views.StreamerList exposing (StreamerListMsg(..), streamerListPageSteps, streamerListView)

import Css
import Error exposing (Error)
import Html.Styled exposing (Html, a, button, div, hr, img, input, label, p, span, text)
import Html.Styled.Attributes exposing (css, href, placeholder, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Icons
import RefreshData exposing (RefreshData)
import Tailwind.Utilities as Tw
import Twitch
import Utils exposing (filterFollowsByLogin)
import Views.Components exposing (errorView, loadingSpinner)


type StreamerListMsg
    = ShowMore
    | ShowLess
    | Filter String
    | ClearFilterString
    | SetStreamerSelection Twitch.User Bool


streamerListPageSteps : Int
streamerListPageSteps =
    10


streamerListView : RefreshData Error (List ( Twitch.User, Bool )) -> List Twitch.FollowRelation -> Int -> Maybe String -> Html StreamerListMsg
streamerListView streamersData follows showCount filterString =
    let
        streamers =
            RefreshData.unwrap streamersData

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
            css [ Tw.text_primary_standalone, Tw.underline ]

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
                            div [ css [ Tw.mt_2, Tw.mx_2 ] ] [ errorView (Error.toString error) ]

                        Nothing ->
                            text ""
                )
                streamersData

        filterUI =
            div
                [ css [ Tw.form_control, Tw.relative, Tw.m_2 ] ]
                [ label
                    [ css [ Tw.label ]
                    ]
                    [ span [ css [ Tw.label_text ] ] [ text "Search" ]
                    ]
                , input
                    [ type_ "text"
                    , value (Maybe.withDefault "" filterString)
                    , placeholder "Channel name"
                    , css
                        [ Tw.input
                        , Tw.input_ghost
                        , Tw.input_bordered
                        , Tw.input_sm
                        , Tw.pr_8
                        ]
                    , onInput (\s -> Filter s)
                    ]
                    []
                , clearTextIcon
                ]

        clearTextIcon =
            case filterString of
                Just _ ->
                    button
                        [ css
                            [ Tw.absolute
                            , Tw.bottom_1
                            , Tw.right_2
                            ]
                        , onClick ClearFilterString
                        ]
                        [ Icons.close
                            [ Tw.w_6
                            , Tw.text_dark_200
                            , Tw.fill_current
                            ]
                        ]

                Nothing ->
                    text ""

        filteredList =
            case filterString of
                Just query ->
                    let
                        divider =
                            [ hr [ css [ Tw.my_2 ] ] [] ]
                    in
                    if String.length query >= 4 then
                        -- this is also computed in update, so we could save us the computation here
                        let
                            list =
                                filterFollowsByLogin query follows
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

                            result =
                                if List.length list > 0 then
                                    list

                                else
                                    [ div [ css [ Tw.text_center ] ] [ text "No channels found" ] ]
                        in
                        result
                            ++ divider

                    else if String.length query > 0 then
                        div [ css [ Tw.text_center ] ] [ text "Enter at least 4 characters" ] :: divider

                    else
                        [ text "" ]

                Nothing ->
                    [ text "" ]

        filterResultsView =
            if List.isEmpty filteredList then
                text ""

            else
                div [ css [] ]
                    filteredList

        selectedView =
            if List.isEmpty selectedStreamers then
                text ""

            else
                div []
                    ((selectedStreamers
                        |> List.map (\( selected, streamer ) -> streamerView selected streamer)
                     )
                        ++ [ hr [ css [ Tw.my_2 ] ] [] ]
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
            , Tw.px_2
            , Tw.py_1
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
            ([ a [ href ("https://twitch.tv/" ++ streamer.displayName) ] [ avatar ]
             , div [ css [ Tw.font_medium, Tw.truncate ] ] [ text streamer.displayName ]
             ]
                ++ (if isSelected then
                        [ Icons.checkCircle
                            [ Tw.w_6
                            , Tw.text_green_400
                            , Tw.fill_current
                            ]
                        ]

                    else
                        []
                   )
            )
        ]

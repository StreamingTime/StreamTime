module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Css
import Css.Global
import Error exposing (Error(..))
import Html.Styled as Html exposing (Html, a, button, div, h1, img, li, p, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (alt, class, classList, css, href, src, style, tabindex)
import Html.Styled.Events exposing (onClick)
import Http
import Icons
import Json.Encode as Encode
import Loading
import LocalStorage
import RefreshData exposing (RefreshData(..))
import Tailwind.Utilities as Tw
import Task
import Time
import Time.Extra
import Twitch
import TwitchConfig
import Types exposing (AppData, SignedInUser, Tab(..))
import Url
import Url.Builder
import Utils exposing (filterFollowsByLogin, missingProfileLogins, streamersWithSelection)
import Views.Calendar exposing (calendarView)
import Views.Components exposing (errorView, loadingSpinner)
import Views.StreamerList exposing (StreamerListMsg(..), streamerListPageSteps, streamerListView)
import Views.Video


type Model
    = {- User is logged in, token is verified -} LoggedIn AppData UrlInfo
    | {- User has not started the login process -} NotLoggedIn (Maybe Error) UrlInfo
    | {- User has logged in via twitch, but we have yet to validate the token and fetch user details -} LoadingScreen UrlInfo


type alias UrlInfo =
    { navKey : Nav.Key
    , rootUrl : String
    }


type Msg
    = UrlMsg UrlMsg
    | GotValidateTokenResponse (Result Http.Error Twitch.ValidateTokenResponse)
    | GotRevokeTokenResponse
    | GotStreamerProfilesForSidebar (Result Http.Error (List Twitch.User))
    | GotStreamerProfiles (Result Http.Error (List Twitch.User))
    | GotStreamingSchedule (Result Error Twitch.Schedule)
    | StreamerListMsg StreamerListMsg
    | Logout
    | HourlyValidation
    | LoadingFinished (Result Http.Error AppData)
    | GotVideos (Result Error (List Twitch.Video))
    | SwitchTab Tab


type UrlMsg
    = LinkClicked Browser.UrlRequest
    | UrlChanged


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        urlInfo =
            { navKey = navKey
            , rootUrl =
                let
                    rootUrl =
                        -- we need an absolute Url for the redirect
                        Url.Builder.crossOrigin
                            (Utils.protocolToString url.protocol
                                ++ url.host
                                ++ Maybe.withDefault ""
                                    (url.port_
                                        |> Maybe.map (\p -> ":" ++ String.fromInt p)
                                    )
                            )
                            []
                            []
                in
                -- remove trailing whitespace for the Twitch return url allowlist
                if String.endsWith "/" rootUrl then
                    String.dropRight 1 rootUrl

                else
                    rootUrl
            }
    in
    case Twitch.accessTokenFromUrl url of
        Just token ->
            ( LoadingScreen urlInfo
            , Cmd.batch
                [ Cmd.map LoadingFinished (Loading.loadInitialData token streamerListPageSteps)
                , LocalStorage.persistData { token = Twitch.getTokenValue token }
                , Nav.replaceUrl navKey "/"
                ]
            )

        Nothing ->
            case LocalStorage.decodePersistentData flags of
                Err _ ->
                    ( NotLoggedIn Nothing urlInfo, Cmd.none )

                Ok data ->
                    let
                        token =
                            Twitch.Token data.token
                    in
                    ( LoadingScreen urlInfo
                    , Cmd.batch
                        [ Cmd.map LoadingFinished (Loading.loadInitialData token streamerListPageSteps)
                        , Nav.replaceUrl urlInfo.navKey "/"
                        ]
                    )


revokeToken : Twitch.ClientID -> Twitch.Token -> Cmd Msg
revokeToken clientId token =
    Cmd.map (\_ -> GotRevokeTokenResponse) (Twitch.revokeToken clientId token)


fetchStreamerProfilesForSidebar : List Twitch.UserID -> Twitch.Token -> Cmd Msg
fetchStreamerProfilesForSidebar userIDs token =
    Cmd.map GotStreamerProfilesForSidebar (Twitch.getUsers userIDs TwitchConfig.clientId token)


fetchStreamerProfiles : List Twitch.UserID -> Twitch.Token -> Cmd Msg
fetchStreamerProfiles userIDs token =
    Cmd.map GotStreamerProfiles (Twitch.getUsers userIDs TwitchConfig.clientId token)


fetchStreamingSchedule : Twitch.UserID -> Time.Posix -> Twitch.Token -> Cmd Msg
fetchStreamingSchedule userID time token =
    let
        endTime =
            Time.Extra.timeInOneWeek time

        fetchSchedulePage cursor =
            Twitch.getStreamingSchedule userID (Time.Extra.onlyDate time |> Just) cursor TwitchConfig.clientId token
    in
    fetchSchedulePage Nothing
        |> Task.andThen
            (Twitch.pagesWhile
                fetchSchedulePage
                (\schedule ->
                    List.all
                        (\segment ->
                            Time.posixToMillis segment.startTime <= Time.posixToMillis endTime
                        )
                        schedule.segments
                )
                []
            )
        |> Task.andThen
            (\scheduleList ->
                Task.mapError Error.StringError
                    (case scheduleList of
                        h :: _ ->
                            Task.succeed
                                { h
                                    | segments =
                                        scheduleList
                                            |> List.concatMap .segments
                                }

                        _ ->
                            Task.fail "did not get a schedule, but response was not 404 either"
                    )
            )
        |> Task.attempt GotStreamingSchedule


fetchVideos : Int -> Twitch.Token -> Twitch.UserID -> Cmd Msg
fetchVideos count token userID =
    Cmd.map GotVideos (Twitch.fetchVideos count userID TwitchConfig.clientId token)


{-| Fetch schedules for every streamer that is selected, but whose schedule is not in the list
-}
fetchMissingSchedules : AppData -> Cmd Msg
fetchMissingSchedules { selectedStreamers, schedules, signedInUser, time } =
    Utils.missingStreamersInSchedules selectedStreamers (RefreshData.unwrap schedules)
        |> List.map
            (\s ->
                fetchStreamingSchedule s.id time signedInUser.token
            )
        |> Cmd.batch


{-| Fetch videos for every streamer that is selected, but whose videos are not in the list
-}
fetchMissingVideos : AppData -> Cmd Msg
fetchMissingVideos { selectedStreamers, videos, signedInUser } =
    Utils.missingStreamersInVideos selectedStreamers (RefreshData.unwrap videos)
        |> List.map (\s -> fetchVideos 10 signedInUser.token s.id)
        |> Cmd.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        LoadingScreen urlInfo ->
            case msg of
                LoadingFinished (Ok appData) ->
                    ( LoggedIn appData urlInfo, Cmd.none )

                LoadingFinished (Err error) ->
                    ( NotLoggedIn (Just (Error.HttpError error)) urlInfo, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoggedIn appData urlInfo ->
            case msg of
                UrlMsg urlMsg ->
                    ( LoggedIn appData urlInfo, handleUrlMsg urlMsg urlInfo.navKey )

                GotValidateTokenResponse (Err err) ->
                    ( NotLoggedIn (Just (HttpError err)) urlInfo, Cmd.none )

                GotValidateTokenResponse (Ok _) ->
                    ( model, Cmd.none )

                GotRevokeTokenResponse ->
                    ( model, Cmd.none )

                GotStreamerProfilesForSidebar response ->
                    case response of
                        Ok newProfiles ->
                            ( LoggedIn
                                { appData
                                    | streamers = RefreshData.map (\oldProfiles -> Present (oldProfiles ++ newProfiles)) appData.streamers
                                    , sidebarStreamerCount = appData.sidebarStreamerCount + List.length newProfiles
                                }
                                urlInfo
                            , Cmd.none
                            )

                        Err err ->
                            ( LoggedIn
                                { appData
                                    | streamers = RefreshData.map (RefreshData.ErrorWithData (HttpError err)) appData.streamers
                                }
                                urlInfo
                            , Cmd.none
                            )

                StreamerListMsg ShowLess ->
                    let
                        newItemCount =
                            max (appData.sidebarStreamerCount - streamerListPageSteps) streamerListPageSteps
                    in
                    ( LoggedIn { appData | sidebarStreamerCount = newItemCount } urlInfo, Cmd.none )

                StreamerListMsg ShowMore ->
                    let
                        loadMore =
                            (appData.sidebarStreamerCount + streamerListPageSteps) > RefreshData.mapTo (\_ -> List.length) appData.streamers
                    in
                    if loadMore then
                        let
                            nextIDs =
                                appData.follows
                                    |> List.drop appData.sidebarStreamerCount
                                    |> List.take streamerListPageSteps
                                    |> List.map .toID
                        in
                        ( LoggedIn { appData | streamers = RefreshData.map RefreshData.LoadingMore appData.streamers } urlInfo
                        , fetchStreamerProfilesForSidebar nextIDs appData.signedInUser.token
                        )

                    else
                        let
                            numProfiles =
                                RefreshData.mapTo (\_ -> List.length) appData.streamers

                            howMuchMore =
                                min streamerListPageSteps (numProfiles - appData.sidebarStreamerCount)
                        in
                        ( LoggedIn { appData | sidebarStreamerCount = appData.sidebarStreamerCount + howMuchMore } urlInfo, Cmd.none )

                StreamerListMsg (Filter name) ->
                    let
                        -- filter follows by name and find out which profiles are missing
                        searchResultsWithoutProfile =
                            appData.follows
                                |> filterFollowsByLogin name
                                |> (\f -> missingProfileLogins f (RefreshData.unwrap appData.streamers))
                    in
                    ( LoggedIn { appData | streamerFilterName = Just name } urlInfo
                    , if String.length name >= 4 && List.length searchResultsWithoutProfile > 0 then
                        fetchStreamerProfiles searchResultsWithoutProfile appData.signedInUser.token

                      else
                        Cmd.none
                    )

                StreamerListMsg ClearFilterString ->
                    ( LoggedIn { appData | streamerFilterName = Nothing } urlInfo, Cmd.none )

                StreamerListMsg (SetStreamerSelection streamer newSelectionState) ->
                    let
                        newList =
                            if newSelectionState then
                                appData.selectedStreamers ++ [ streamer ]

                            else
                                List.filter ((/=) streamer) appData.selectedStreamers

                        newAppData =
                            { appData | schedules = RefreshData.map LoadingMore appData.schedules, selectedStreamers = newList, streamerFilterName = Nothing }
                    in
                    ( LoggedIn newAppData urlInfo
                    , case appData.tab of
                        ScheduleTab ->
                            fetchMissingSchedules newAppData

                        VideoTab ->
                            fetchMissingVideos newAppData
                    )

                GotStreamerProfiles (Ok newProfiles) ->
                    ( LoggedIn { appData | streamers = RefreshData.map (\oldProfiles -> Present (oldProfiles ++ newProfiles)) appData.streamers } urlInfo, Cmd.none )

                GotStreamerProfiles (Err err) ->
                    ( LoggedIn { appData | streamers = RefreshData.map (ErrorWithData (HttpError err)) appData.streamers } urlInfo, Cmd.none )

                GotStreamingSchedule response ->
                    case response of
                        Ok value ->
                            ( LoggedIn { appData | schedules = RefreshData.map (\oldSchedules -> Present (oldSchedules ++ [ value ])) appData.schedules } urlInfo, Cmd.none )

                        -- Twitch responds with 404 when a streamer has no schedule
                        Err (HttpError (Http.BadStatus 404)) ->
                            ( LoggedIn { appData | schedules = RefreshData.map Present appData.schedules } urlInfo, Cmd.none )

                        Err err ->
                            ( LoggedIn { appData | schedules = RefreshData.map (ErrorWithData err) appData.schedules } urlInfo, Cmd.none )

                Logout ->
                    ( NotLoggedIn Nothing urlInfo, Cmd.batch [ LocalStorage.removeData, revokeToken TwitchConfig.clientId appData.signedInUser.token ] )

                HourlyValidation ->
                    ( model, Cmd.map GotValidateTokenResponse (Twitch.validateToken appData.signedInUser.token) )

                GotVideos (Ok response) ->
                    ( LoggedIn { appData | videos = RefreshData.map (\v -> Present (v ++ response)) appData.videos } urlInfo, Cmd.none )

                GotVideos (Err e) ->
                    ( LoggedIn { appData | videos = RefreshData.map (ErrorWithData e) appData.videos } urlInfo, Cmd.none )

                LoadingFinished _ ->
                    ( model, Cmd.none )

                SwitchTab newTab ->
                    ( LoggedIn { appData | tab = newTab } urlInfo
                    , case newTab of
                        ScheduleTab ->
                            fetchMissingSchedules appData

                        VideoTab ->
                            fetchMissingVideos appData
                    )

        NotLoggedIn _ urlInfo ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg urlInfo.navKey )

                _ ->
                    ( model, Cmd.none )


handleUrlMsg : UrlMsg -> Nav.Key -> Cmd Msg
handleUrlMsg msg navKey =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    Nav.pushUrl navKey (Url.toString url)

                Browser.External href ->
                    Nav.load href

        UrlChanged ->
            Cmd.none


view : Model -> Document Msg
view model =
    { title = "Stream Time"
    , body =
        [ toUnstyled <|
            div []
                [ Css.Global.global Tw.globalStyles
                , div []
                    [ case model of
                        NotLoggedIn err urlInfo ->
                            loginView err urlInfo.rootUrl

                        LoadingScreen _ ->
                            validationView

                        LoggedIn appData _ ->
                            appView appData
                    ]
                ]
        ]
    }


loginView : Maybe Error -> String -> Html Msg
loginView err loginRedirectUrl =
    div
        [ css
            [ Tw.hero
            , Tw.min_h_screen
            ]
        , style "background-image" "url('assets/login_background.jpg')"
        ]
        [ div
            [ css
                [ Tw.text_center
                , Tw.hero_content
                ]
            ]
            [ div
                [ css
                    [ Tw.max_w_md
                    ]
                ]
                [ h1
                    [ css
                        [ Tw.mb_8
                        , Tw.text_5xl
                        , Tw.font_bold
                        ]
                    ]
                    [ text "Stream ", span [ css [ Tw.text_purple_400 ] ] [ text "Time" ] ]
                , p
                    [ css
                        [ Tw.mb_8
                        , Tw.font_semibold
                        ]
                    ]
                    [ text "All schedules from your favorite streamers in one place." ]
                , a
                    [ href (Twitch.loginFlowUrl TwitchConfig.clientId loginRedirectUrl) ]
                    [ button
                        [ css
                            [ Tw.btn
                            , Tw.btn_primary
                            , Css.hover [ Tw.bg_primary_focus ]
                            ]
                        ]
                        [ text "Login with twitch" ]
                    ]
                , case err of
                    Just e ->
                        div [ css [ Tw.mt_8 ] ]
                            [ errorView (Error.toString e) ]

                    Nothing ->
                        text ""
                ]
            ]
        ]


validationView : Html Msg
validationView =
    div
        [ css
            [ Tw.h_screen
            , Tw.flex
            , Tw.items_center
            , Tw.justify_center
            ]
        ]
        [ div
            []
            [ loadingSpinner [ Tw.w_16, Tw.h_16 ] ]
        ]


appView : AppData -> Html Msg
appView appData =
    let
        tabButtons =
            div
                [ css
                    [ Tw.tabs
                    , Tw.tabs_boxed
                    , Tw.max_w_max
                    ]
                ]
                [ div
                    [ css [ Tw.tab, Tw.w_20 ]
                    , classList [ ( "tab-active", appData.tab == ScheduleTab ) ]
                    , onClick (SwitchTab ScheduleTab)
                    ]
                    [ text "Schedule" ]
                , div
                    [ css [ Tw.tab, Tw.w_20 ]
                    , classList [ ( "tab-active", appData.tab == VideoTab ) ]
                    , onClick (SwitchTab VideoTab)
                    ]
                    [ text "Videos" ]
                ]
    in
    div []
        [ headerView appData.signedInUser
        , div [ css [ Tw.flex ] ]
            [ Html.map (\msg -> StreamerListMsg msg)
                (streamerListView
                    (RefreshData.mapValue
                        (streamersWithSelection appData.selectedStreamers)
                        appData.streamers
                    )
                    appData.follows
                    appData.sidebarStreamerCount
                    appData.streamerFilterName
                )
            , div
                [ css
                    [ Tw.bg_base_100
                    , Tw.w_full
                    , Tw.ml_60
                    , Tw.mt_16
                    ]
                ]
                [ div
                    [ css
                        [ Tw.flex
                        , Tw.justify_center
                        , Tw.flex_col
                        , Tw.items_center
                        , Tw.mt_4
                        ]
                    ]
                    [ tabButtons
                    , if List.isEmpty appData.selectedStreamers then
                        div
                            [ css
                                ([ Tw.alert
                                 , Tw.alert_info
                                 , Tw.mt_2
                                 ]
                                    ++ (case appData.tab of
                                            VideoTab ->
                                                []

                                            ScheduleTab ->
                                                [ Tw.absolute ]
                                       )
                                )
                            ]
                            [ div
                                [ css
                                    [ Tw.flex
                                    , Tw.items_center
                                    , Tw.space_x_2
                                    ]
                                ]
                                [ Icons.info
                                    [ Tw.h_5
                                    , Tw.fill_current
                                    , Tw.text_blue_300
                                    ]
                                , p [] [ text "Select channels from the left to view schedules and videos." ]
                                ]
                            ]

                      else
                        text ""
                    , case appData.tab of
                        VideoTab ->
                            videoTabView appData

                        ScheduleTab ->
                            scheduleTabView appData
                    ]
                ]
            ]
        ]


scheduleTabView : AppData -> Html Msg
scheduleTabView appData =
    RefreshData.mapTo
        (\err _ ->
            case err of
                Just error ->
                    errorView (Error.toString error)

                Nothing ->
                    div
                        [ css [ Tw.w_5over6, Tw.py_10 ] ]
                        [ calendarView appData.timeZone appData.time appData.streamers appData.schedules appData.selectedStreamers ]
        )
        appData.schedules


videoTabView : AppData -> Html Msg
videoTabView { selectedStreamers, videos } =
    div [ css [ Tw.self_start ] ]
        (case videos of
            RefreshData.ErrorWithData error _ ->
                [ errorView (Error.toString error) ]

            _ ->
                [ videos
                    |> RefreshData.unwrap
                    |> List.filter (\video -> List.any (\streamer -> streamer.id == video.userID) selectedStreamers)
                    |> Views.Video.videoListView
                ]
        )


headerView : SignedInUser -> Html Msg
headerView user =
    div
        [ css
            [ Tw.bg_base_100
            , Tw.border_base_200
            , Tw.border_2
            , Tw.fixed
            , Tw.h_16
            , Tw.w_full
            , Tw.z_10
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.items_center
                , Tw.justify_between
                , Tw.h_full
                , Tw.mx_6
                ]
            ]
            [ div
                [ css
                    [ Tw.text_xl
                    , Tw.font_semibold
                    , Tw.text_white
                    ]
                ]
                [ text "Stream "
                , span [ css [ Tw.text_purple_400 ] ] [ text "Time" ]
                ]
            , userView user
            ]
        ]


userView : SignedInUser -> Html Msg
userView user =
    let
        avatar =
            div [ css [ Tw.avatar, Css.hover [ Tw.cursor_pointer ] ], tabindex 0 ]
                [ div [ css [ Tw.rounded_full, Tw.w_10, Tw.h_10 ] ]
                    [ img [ src user.profileImageUrl, alt (user.displayName ++ " profile image") ] []
                    ]
                ]
    in
    div
        [ classList
            -- we have to add dropdown classes manually for the dropdown to work, this seems to be a daisyUI bug
            [ ( "dropdown", True ), ( "dropdown-end", True ) ]
        , css [ Tw.dropdown, Tw.dropdown_end ]
        ]
        [ avatar
        , ul
            [ tabindex 0
            , css
                [ Tw.mt_1
                , Tw.p_2
                , Tw.shadow
                , Tw.menu
                , Tw.bg_dark_600
                , Tw.rounded_xl
                , Tw.w_36
                ]
            , class "dropdown-content"
            ]
            [ li []
                [ p [ css [ Tw.text_center, Tw.font_semibold ] ] [ text user.displayName ] ]
            , li
                [ css
                    [ Tw.border_t_2
                    , Tw.border_dark_700
                    , Tw.mt_2
                    ]
                ]
                [ button
                    [ css
                        [ Tw.btn
                        , Tw.btn_ghost
                        , Tw.border_0
                        , Css.hover [ Tw.bg_transparent ]
                        ]
                    , onClick Logout
                    ]
                    [ p
                        [ css
                            [ Tw.font_bold
                            , Tw.text_red_500
                            ]
                        ]
                        [ text "Logout" ]
                    ]
                ]
            ]
        ]


main : Program Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Time.every (60 * 60 * 1000) (\_ -> HourlyValidation)
        , onUrlRequest = \urlRequest -> UrlMsg (LinkClicked urlRequest)
        , onUrlChange = \_ -> UrlMsg UrlChanged
        }

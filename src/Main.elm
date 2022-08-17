module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Components exposing (errorView, loadingSpinner)
import Css
import Css.Global
import Error exposing (Error(..))
import Html.Styled as Html exposing (Html, a, button, div, h1, img, li, p, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (alt, class, classList, css, href, src, style, tabindex)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Encode as Encode
import LocalStorage
import RFC3339
import RefreshData exposing (RefreshData(..))
import Tailwind.Utilities as Tw
import Task
import Time
import Time.Extra
import Twitch
import TwitchConfig
import Url
import Utils exposing (filterFollowsByLogin, missingProfileLogins, streamersWithSelection)
import Views.Calendar exposing (calendarView)
import Views.StreamerList exposing (StreamerListMsg(..), streamerListPageSteps, streamerListView)


loginRedirectUrl : String
loginRedirectUrl =
    "http://localhost:8000"


type Model
    = {- User is logged in, token is verified -} LoggedIn AppData Nav.Key
    | {- User has not started the login process -} NotLoggedIn (Maybe Error) Nav.Key
    | {- User has logged in via twitch, but we have yet to validate the token and fetch user details -} LoadingScreen Nav.Key


type alias AppData =
    { signedInUser : SignedInUser
    , streamers : RefreshData Error (List Twitch.User)

    -- a list of follow relation metadata originating from our user
    , follows : List Twitch.FollowRelation
    , sidebarStreamerCount : Int
    , streamerFilterName : Maybe String
    , selectedStreamers : List Twitch.User
    , schedules : RefreshData Error (List Twitch.Schedule)
    , timeZone : Time.Zone
    , time : Time.Posix
    }



{- Data we need or fetch during the loading screen -}


type alias WithVerifiedToken =
    { token : Twitch.Token
    , loginName : String
    , userID : String
    }


type alias WithFullUser =
    SignedInUser


type alias WithTimeInfo =
    { signedInUser : SignedInUser
    , zone : Time.Zone
    , time : Time.Posix
    }


type alias WithFollows =
    { signedInUser : SignedInUser
    , zone : Time.Zone
    , time : Time.Posix
    , follows : List Twitch.FollowRelation
    }


type alias SignedInUser =
    { token : Twitch.Token
    , loginName : String
    , userID : String
    , displayName : Maybe String -- TODO: dont use maybe here
    , profileImageUrl : Maybe String -- TODO: dont use maybe here
    }


validateToken : Twitch.Token -> Task.Task Http.Error WithVerifiedToken
validateToken token =
    Twitch.validateTokenTask token
        |> Task.map
            (\{ login, userID } ->
                { token = token
                , loginName = login
                , userID = userID
                }
            )


getUserInfo : WithVerifiedToken -> Task.Task Http.Error WithFullUser
getUserInfo { token } =
    Twitch.getLoggedInUserTask TwitchConfig.clientId token
        |> Task.map
            (\{ id, displayName, profileImageUrl, loginName } ->
                { token = token
                , userID = id
                , loginName = loginName
                , displayName = Just displayName
                , profileImageUrl = Just profileImageUrl
                }
            )


getTimeStuff : WithFullUser -> Task.Task Http.Error WithTimeInfo
getTimeStuff signedInUser =
    Task.map2
        (\time zone ->
            { signedInUser = signedInUser, time = time, zone = zone }
        )
        Time.now
        Time.here


getFollows : WithTimeInfo -> Task.Task Http.Error WithFollows
getFollows { signedInUser, time, zone } =
    Twitch.getUserFollowsTask signedInUser.userID Nothing TwitchConfig.clientId signedInUser.token
        |> Task.map
            (\page ->
                { signedInUser = signedInUser
                , time = time
                , zone = zone
                , follows = page.data
                }
            )


getFirstStreamerProfiles : WithFollows -> Task.Task Http.Error AppData
getFirstStreamerProfiles { follows, signedInUser, time, zone } =
    let
        streamerIDs =
            List.map .toID follows
    in
    Twitch.getUsersTask streamerIDs TwitchConfig.clientId signedInUser.token
        |> Task.map
            (\streamers ->
                { signedInUser = signedInUser
                , streamers = RefreshData.Present streamers
                , follows = follows
                , timeZone = zone
                , time = time
                , sidebarStreamerCount = streamerListPageSteps
                , streamerFilterName = Nothing
                , schedules = RefreshData.LoadingMore []
                , selectedStreamers = []
                }
            )


loadStuff : Twitch.Token -> Cmd Msg
loadStuff token =
    validateToken token
        |> Task.andThen getUserInfo
        |> Task.andThen getTimeStuff
        |> Task.andThen getFollows
        |> Task.andThen getFirstStreamerProfiles
        |> Task.attempt LoadingFinished


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


type UrlMsg
    = LinkClicked Browser.UrlRequest
    | UrlChanged


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    case Twitch.accessTokenFromUrl url of
        Just token ->
            ( LoadingScreen navKey
            , Cmd.batch
                [ loadStuff token
                , LocalStorage.persistData { token = Twitch.getTokenValue token }
                , Nav.replaceUrl navKey "/"
                ]
            )

        Nothing ->
            case LocalStorage.decodePersistentData flags of
                Err _ ->
                    ( NotLoggedIn Nothing navKey, Cmd.none )

                Ok data ->
                    let
                        token =
                            Twitch.Token data.token
                    in
                    ( LoadingScreen navKey
                    , Cmd.batch [ loadStuff token, Nav.replaceUrl navKey "/" ]
                    )


revokeToken : Twitch.ClientID -> Twitch.Token -> Cmd Msg
revokeToken clientId token =
    Cmd.map (\_ -> GotRevokeTokenResponse) (Twitch.revokeToken clientId token)


fetchStreamerProfilesForSidebar : List String -> Twitch.Token -> Cmd Msg
fetchStreamerProfilesForSidebar userIDs token =
    Cmd.map GotStreamerProfilesForSidebar (Twitch.getUsers userIDs TwitchConfig.clientId token)


fetchStreamerProfiles : List String -> Twitch.Token -> Cmd Msg
fetchStreamerProfiles userIDs token =
    Cmd.map GotStreamerProfiles (Twitch.getUsers userIDs TwitchConfig.clientId token)


fetchStreamingSchedule : String -> Time.Zone -> Time.Posix -> Twitch.Token -> Cmd Msg
fetchStreamingSchedule userID timeZone time token =
    let
        -- get all segments that start before endTime
        beforeEndTime : Time.Posix -> List Twitch.Segment -> List Twitch.Segment
        beforeEndTime endTime =
            List.filter
                (\segment ->
                    case RFC3339.toPosix segment.startTime of
                        Just startTime ->
                            Time.posixToMillis startTime <= Time.posixToMillis endTime

                        Nothing ->
                            False
                )

        -- Fetch the next page until a) we got all segments that start before endTime or b) there are no pages left
        -- scheduleAcc is used to accumulate schedule entries
        fetchMore : Time.Posix -> Maybe String -> Twitch.Schedule -> Task.Task Error Twitch.Schedule
        fetchMore endTime currentCursor scheduleAcc =
            Task.andThen
                (\result ->
                    case result.cursor of
                        Just _ ->
                            if List.length (beforeEndTime endTime result.data.segments) < List.length result.data.segments then
                                Task.succeed { scheduleAcc | segments = scheduleAcc.segments ++ beforeEndTime endTime result.data.segments }

                            else
                                fetchMore endTime result.cursor { scheduleAcc | segments = scheduleAcc.segments ++ beforeEndTime endTime result.data.segments }

                        Nothing ->
                            Task.succeed scheduleAcc
                )
                (Twitch.getStreamingSchedule userID timeZone Nothing currentCursor TwitchConfig.clientId token)

        {- fetch the first page (and more if needed) -}
        startFetching : Time.Posix -> Task.Task Error Twitch.Schedule
        startFetching endTime =
            Twitch.getStreamingSchedule userID timeZone Nothing Nothing TwitchConfig.clientId token
                |> Task.andThen
                    (\{ cursor, data } ->
                        case cursor of
                            Just _ ->
                                if List.length (beforeEndTime endTime data.segments) < List.length data.segments then
                                    Task.succeed { data | segments = beforeEndTime endTime data.segments }

                                else
                                    fetchMore endTime cursor data

                            Nothing ->
                                Task.succeed data
                    )
    in
    startFetching (Time.Extra.timeInOneWeek time)
        |> Task.attempt
            GotStreamingSchedule


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        LoadingScreen navKey ->
            case msg of
                LoadingFinished (Ok appData) ->
                    ( LoggedIn appData navKey, Cmd.none )

                LoadingFinished (Err error) ->
                    ( NotLoggedIn (Just (Error.HttpError error)) navKey, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoggedIn appData navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( LoggedIn appData navKey, handleUrlMsg urlMsg navKey )

                GotValidateTokenResponse (Err err) ->
                    ( NotLoggedIn (Just (HttpError err)) navKey, Cmd.none )

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
                                navKey
                            , Cmd.none
                            )

                        Err err ->
                            ( LoggedIn
                                { appData
                                    | streamers = RefreshData.map (RefreshData.ErrorWithData (HttpError err)) appData.streamers
                                }
                                navKey
                            , Cmd.none
                            )

                StreamerListMsg ShowLess ->
                    let
                        newItemCount =
                            max (appData.sidebarStreamerCount - streamerListPageSteps) streamerListPageSteps
                    in
                    ( LoggedIn { appData | sidebarStreamerCount = newItemCount } navKey, Cmd.none )

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
                        ( LoggedIn { appData | streamers = RefreshData.map RefreshData.LoadingMore appData.streamers } navKey
                        , fetchStreamerProfilesForSidebar nextIDs appData.signedInUser.token
                        )

                    else
                        let
                            numProfiles =
                                RefreshData.mapTo (\_ -> List.length) appData.streamers

                            howMuchMore =
                                min streamerListPageSteps (numProfiles - appData.sidebarStreamerCount)
                        in
                        ( LoggedIn { appData | sidebarStreamerCount = appData.sidebarStreamerCount + howMuchMore } navKey, Cmd.none )

                StreamerListMsg (Filter name) ->
                    let
                        -- filter follows by name and find out which profiles are missing
                        searchResultsWithoutProfile =
                            appData.follows
                                |> filterFollowsByLogin name
                                |> (\f -> missingProfileLogins f (RefreshData.mapTo (\_ v -> v) appData.streamers))
                    in
                    ( LoggedIn { appData | streamerFilterName = Just name } navKey
                    , if String.length name >= 4 && List.length searchResultsWithoutProfile > 0 then
                        fetchStreamerProfiles searchResultsWithoutProfile appData.signedInUser.token

                      else
                        Cmd.none
                    )

                StreamerListMsg ClearFilterString ->
                    ( LoggedIn { appData | streamerFilterName = Nothing } navKey, Cmd.none )

                StreamerListMsg (SetStreamerSelection streamer newSelectionState) ->
                    let
                        newList =
                            if newSelectionState then
                                appData.selectedStreamers ++ [ streamer ]

                            else
                                List.filter ((/=) streamer) appData.selectedStreamers
                    in
                    ( LoggedIn { appData | schedules = RefreshData.map LoadingMore appData.schedules, selectedStreamers = newList } navKey
                    , Utils.missingStreamersInSchedules newList (RefreshData.mapTo (\_ -> identity) appData.schedules)
                        |> List.map
                            (\s ->
                                fetchStreamingSchedule s.id appData.timeZone appData.time appData.signedInUser.token
                            )
                        |> Cmd.batch
                    )

                GotStreamerProfiles (Ok newProfiles) ->
                    ( LoggedIn { appData | streamers = RefreshData.map (\oldProfiles -> Present (oldProfiles ++ newProfiles)) appData.streamers } navKey, Cmd.none )

                GotStreamerProfiles (Err err) ->
                    ( LoggedIn { appData | streamers = RefreshData.map (ErrorWithData (HttpError err)) appData.streamers } navKey, Cmd.none )

                GotStreamingSchedule response ->
                    case response of
                        Err err ->
                            ( LoggedIn { appData | schedules = RefreshData.map (ErrorWithData err) appData.schedules } navKey, Cmd.none )

                        Ok value ->
                            ( LoggedIn { appData | schedules = RefreshData.map (\oldSchedules -> Present (oldSchedules ++ [ value ])) appData.schedules } navKey, Cmd.none )

                Logout ->
                    ( NotLoggedIn Nothing navKey, Cmd.batch [ LocalStorage.removeData, revokeToken TwitchConfig.clientId appData.signedInUser.token ] )

                HourlyValidation ->
                    ( model, Cmd.map GotValidateTokenResponse (Twitch.validateToken appData.signedInUser.token) )

                LoadingFinished _ ->
                    ( model, Cmd.none )

        NotLoggedIn _ navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

                -- this msg should not be relevant for the LoadingScreen model
                GotValidateTokenResponse _ ->
                    ( model, Cmd.none )

                GotRevokeTokenResponse ->
                    ( model, Cmd.none )

                GotStreamerProfilesForSidebar _ ->
                    ( model, Cmd.none )

                StreamerListMsg _ ->
                    ( model, Cmd.none )

                GotStreamerProfiles _ ->
                    ( model, Cmd.none )

                GotStreamingSchedule _ ->
                    ( model, Cmd.none )

                Logout ->
                    ( model, Cmd.none )

                HourlyValidation ->
                    ( model, Cmd.none )

                LoadingFinished _ ->
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
    { title = "Twitch schedule"
    , body =
        [ toUnstyled <|
            div []
                [ Css.Global.global Tw.globalStyles
                , div []
                    [ case model of
                        NotLoggedIn err _ ->
                            loginView err

                        LoadingScreen _ ->
                            validationView

                        LoggedIn appData _ ->
                            appView appData
                    ]
                ]
        ]
    }


loginView : Maybe Error -> Html Msg
loginView err =
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
                    [ text "Twitch ", span [ css [ Tw.text_purple_400 ] ] [ text "Schedule" ] ]
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
                    , Tw.flex
                    , Tw.flex_col
                    , Tw.items_center
                    , Tw.space_y_4
                    ]
                ]
                [ div [ css [ Tw.w_5over6, Tw.py_10 ] ]
                    [ calendarView appData.timeZone appData.time appData.streamers appData.schedules appData.selectedStreamers ]
                ]
            ]
        ]


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
                [ text "Twitch "
                , span [ css [ Tw.text_purple_400 ] ] [ text "Schedule" ]
                ]
            , userView user
            ]
        ]


userView : SignedInUser -> Html Msg
userView user =
    let
        imgUrl =
            case user.profileImageUrl of
                Just url ->
                    url

                Nothing ->
                    ""

        name =
            case user.displayName of
                Just displayName ->
                    displayName

                Nothing ->
                    user.loginName

        avatar =
            div [ css [ Tw.avatar, Css.hover [ Tw.cursor_pointer ] ], tabindex 0 ]
                [ div [ css [ Tw.rounded_full, Tw.w_10, Tw.h_10 ] ]
                    [ img [ src imgUrl, alt (name ++ " profile image") ] []
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
                [ p [ css [ Tw.text_center, Tw.font_semibold ] ] [ text name ] ]
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

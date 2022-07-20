module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Css
import Css.Global
import Html.Styled exposing (Html, a, button, div, h1, hr, img, input, label, li, p, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (alt, class, classList, css, href, placeholder, src, style, tabindex, type_)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import LocalStorage
import RefreshData exposing (RefreshData(..))
import Tailwind.Utilities as Tw
import Twitch
import TwitchConfig
import Url
import Utils exposing (filterFollowsByLogin, missingProfileLogins, streamersWithSelection)


loginRedirectUrl : String
loginRedirectUrl =
    "http://localhost:8000"


type Model
    = {- User is logged in, token is verified -} LoggedIn AppData Nav.Key
    | {- User has not started the login process -} NotLoggedIn (Maybe Http.Error) Nav.Key
    | {- User has logged in via twitch, but we have yet to validate the token and fetch user details -} LoadingScreen LoadingData Nav.Key


type alias AppData =
    { signedInUser : SignedInUser
    , streamers : RefreshData Http.Error (List Twitch.User)

    -- a list of follow relation metadata originating from our user
    , follows : List Twitch.FollowRelation
    , sidebarStreamerCount : Int
    , streamerFilterName : Maybe String
    , selectedStreamers : List Twitch.User
    , error : Maybe String
    , schedules : RefreshData Http.Error (List Twitch.Schedule)
    }



{- Data we need or fetch during the loading screen -}


type alias LoadingData =
    { token : Twitch.Token
    , follows : Maybe (List Twitch.FollowRelation)
    , signedInUser : Maybe SignedInUser

    -- the first n streamers to display in the streamer list
    , firstStreamers : Maybe (List Twitch.User)
    }


type alias SignedInUser =
    { token : Twitch.Token
    , loginName : String
    , userID : String
    , displayName : Maybe String
    , profileImageUrl : Maybe String
    }


type Msg
    = UrlMsg UrlMsg
    | GotValidateTokenResponse (Result Http.Error Twitch.ValidateTokenResponse)
    | GotRevokeTokenResponse
    | GotUserFollows (Result Http.Error (Twitch.PaginatedResponse (List Twitch.FollowRelation)))
    | GotStreamerProfilesForSidebar (Result Http.Error (List Twitch.User))
    | GotStreamerProfiles (Result Http.Error (List Twitch.User))
    | GotUserProfile (Result Http.Error Twitch.User)
    | GotStreamingSchedule (Result Http.Error (Twitch.PaginatedResponse Twitch.Schedule))
    | FetchStreamingSchedules
    | StreamerListMsg StreamerListMsg
    | Logout


type StreamerListMsg
    = ShowMore
    | ShowLess
    | Filter String
    | SetStreamerSelection Twitch.User Bool


type UrlMsg
    = LinkClicked Browser.UrlRequest
    | UrlChanged


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    case Twitch.accessTokenFromUrl url of
        Just token ->
            ( LoadingScreen
                { token = token
                , follows = Nothing
                , signedInUser = Nothing
                , firstStreamers = Nothing
                }
                navKey
            , Cmd.batch [ Cmd.map GotValidateTokenResponse (Twitch.validateToken token), Nav.replaceUrl navKey "/" ]
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
                    ( LoadingScreen
                        { token = token
                        , follows = Nothing
                        , signedInUser = Nothing
                        , firstStreamers = Nothing
                        }
                        navKey
                    , Cmd.batch [ Cmd.map GotValidateTokenResponse (Twitch.validateToken token), Nav.replaceUrl navKey "/" ]
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


fetchUserProfile : String -> Twitch.Token -> Cmd Msg
fetchUserProfile userID token =
    Cmd.map GotUserProfile (Twitch.getUser userID TwitchConfig.clientId token)


fetchStreamingSchedule : String -> Twitch.Token -> Cmd Msg
fetchStreamingSchedule userID token =
    Cmd.map GotStreamingSchedule (Twitch.getStreamingSchedule userID Nothing TwitchConfig.clientId token)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        LoadingScreen m navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

                GotValidateTokenResponse response ->
                    case response of
                        Err err ->
                            ( NotLoggedIn (Just err) navKey, Cmd.none )

                        Ok value ->
                            ( LoadingScreen
                                { token = m.token
                                , signedInUser =
                                    Just
                                        { token = m.token
                                        , loginName = value.login
                                        , userID = value.userID
                                        , displayName = Nothing
                                        , profileImageUrl = Nothing
                                        }
                                , follows = Nothing
                                , firstStreamers = Nothing
                                }
                                navKey
                            , Cmd.batch [ LocalStorage.persistData { token = Twitch.getTokenValue m.token }, fetchUserProfile value.userID m.token ]
                            )

                GotRevokeTokenResponse ->
                    ( model, Cmd.none )

                GotUserFollows response ->
                    case ( m.signedInUser, response ) of
                        ( _, Err e ) ->
                            ( NotLoggedIn (Just e) navKey, Cmd.none )

                        ( Just user, Ok paginatedResponse ) ->
                            let
                                -- append the new page to the axisting values, if any
                                oldAndNewValues =
                                    Utils.concatMaybeList m.follows paginatedResponse.data

                                -- use the cursor from the response to fetch the next page
                                nextPage : String -> Cmd Msg
                                nextPage cursor =
                                    Cmd.map GotUserFollows (Twitch.getUserFollows user.userID (Just cursor) TwitchConfig.clientId user.token)
                            in
                            case paginatedResponse.cursor of
                                -- if there are more follows to load, fetch the next page
                                Just cursor ->
                                    ( LoadingScreen { m | follows = Just oldAndNewValues } navKey
                                    , nextPage cursor
                                    )

                                -- after all follows are fetched, fetch the first streamer profiles
                                Nothing ->
                                    ( LoadingScreen { m | follows = Just oldAndNewValues } navKey
                                    , fetchStreamerProfilesForSidebar (List.map .toID (List.take streamerListPageSteps oldAndNewValues)) user.token
                                    )

                        ( Nothing, Ok _ ) ->
                            Debug.todo "this case should not happen"

                GotStreamerProfilesForSidebar response ->
                    case ( m.signedInUser, m.follows, response ) of
                        ( Just user, Just follows, Ok streamers ) ->
                            -- all good, loading is complete
                            ( LoggedIn
                                { signedInUser = user
                                , follows = follows
                                , streamers = Present streamers

                                -- TOOD: sidebarStreamerCount should depend on the number of streamers loaded
                                , sidebarStreamerCount = streamerListPageSteps
                                , selectedStreamers = []
                                , error = Nothing
                                , streamerFilterName = Nothing
                                , schedules = LoadingMore []
                                }
                                navKey
                            , Cmd.none
                            )

                        ( _, _, Err e ) ->
                            ( NotLoggedIn (Just e) navKey, Cmd.none )

                        _ ->
                            Debug.todo "this case should not happen, since we cant fetch profiles if user or follows are unknown"

                GotUserProfile response ->
                    case ( m.signedInUser, response ) of
                        ( Just user, Ok profile ) ->
                            let
                                updatedUser =
                                    { user | displayName = Just profile.displayName, profileImageUrl = Just profile.profileImageUrl }
                            in
                            ( LoadingScreen { m | signedInUser = Just updatedUser } navKey
                            , Cmd.map GotUserFollows (Twitch.getUserFollows user.userID Nothing TwitchConfig.clientId m.token)
                            )

                        ( _, Err e ) ->
                            ( NotLoggedIn (Just e) navKey, Cmd.none )

                        ( Nothing, _ ) ->
                            Debug.todo "again, a case that should not happen"

                GotStreamingSchedule _ ->
                    ( model, Cmd.none )

                FetchStreamingSchedules ->
                    ( model, Cmd.none )

                StreamerListMsg _ ->
                    ( model, Cmd.none )

                GotStreamerProfiles _ ->
                    ( model, Cmd.none )

                Logout ->
                    ( model, Cmd.none )

        LoggedIn appData navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( LoggedIn appData navKey, handleUrlMsg urlMsg navKey )

                GotValidateTokenResponse _ ->
                    ( model, Cmd.none )

                GotRevokeTokenResponse ->
                    ( model, Cmd.none )

                GotUserFollows _ ->
                    ( model, Cmd.none )

                GotUserProfile _ ->
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
                                    | streamers = RefreshData.map (RefreshData.ErrorWithData err) appData.streamers
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

                StreamerListMsg (SetStreamerSelection streamer newSelectionState) ->
                    let
                        newList =
                            if newSelectionState then
                                appData.selectedStreamers ++ [ streamer ]

                            else
                                List.filter ((/=) streamer) appData.selectedStreamers
                    in
                    ( LoggedIn { appData | selectedStreamers = newList } navKey, Cmd.none )

                GotStreamerProfiles (Ok newProfiles) ->
                    ( LoggedIn { appData | streamers = RefreshData.map (\oldProfiles -> Present (oldProfiles ++ newProfiles)) appData.streamers } navKey, Cmd.none )

                GotStreamerProfiles (Err err) ->
                    ( LoggedIn { appData | streamers = RefreshData.map (ErrorWithData err) appData.streamers } navKey, Cmd.none )

                GotStreamingSchedule response ->
                    case response of
                        Err err ->
                            ( LoggedIn { appData | schedules = RefreshData.map (ErrorWithData err) appData.schedules } navKey, Cmd.none )

                        Ok value ->
                            ( LoggedIn { appData | schedules = RefreshData.map (\oldSchedules -> Present (oldSchedules ++ [ value.data ])) appData.schedules } navKey, Cmd.none )

                FetchStreamingSchedules ->
                    ( LoggedIn { appData | schedules = RefreshData.map LoadingMore appData.schedules } navKey
                    , Cmd.batch (List.map (\streamer -> fetchStreamingSchedule streamer.id appData.signedInUser.token) appData.selectedStreamers)
                    )

                Logout ->
                    ( NotLoggedIn Nothing navKey, Cmd.batch [ LocalStorage.removeData, revokeToken TwitchConfig.clientId appData.signedInUser.token ] )

        NotLoggedIn _ navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

                -- this msg should not be relevant for the LoadingScreen model
                GotValidateTokenResponse _ ->
                    ( model, Cmd.none )

                GotRevokeTokenResponse ->
                    ( model, Cmd.none )

                GotUserFollows _ ->
                    ( model, Cmd.none )

                GotStreamerProfilesForSidebar _ ->
                    ( model, Cmd.none )

                StreamerListMsg _ ->
                    ( model, Cmd.none )

                GotUserProfile _ ->
                    ( model, Cmd.none )

                GotStreamerProfiles _ ->
                    ( model, Cmd.none )

                GotStreamingSchedule _ ->
                    ( model, Cmd.none )

                FetchStreamingSchedules ->
                    ( model, Cmd.none )

                Logout ->
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


errorToString : Http.Error -> String
errorToString error =
    let
        networkProblem =
            "Failed to connect to the server. Is your internet ok?"

        generalProblem =
            "There was a problem :("
    in
    case error of
        Http.Timeout ->
            networkProblem

        Http.NetworkError ->
            networkProblem

        Http.BadUrl _ ->
            generalProblem

        Http.BadBody _ ->
            generalProblem

        Http.BadStatus _ ->
            generalProblem


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

                        LoadingScreen _ _ ->
                            validationView

                        LoggedIn appData _ ->
                            appView appData
                    ]
                ]
        ]
    }


loginView : Maybe Http.Error -> Html Msg
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
                        p
                            [ css
                                [ Tw.mt_8
                                , Tw.text_red_500
                                ]
                            ]
                            [ text (errorToString e) ]

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


loadingSpinner : List Css.Style -> Html Msg
loadingSpinner styles =
    div
        [ css
            (List.concat
                [ styles
                , [ Tw.border_4
                  , Tw.border_solid
                  , Tw.border_white
                  , Tw.rounded_full
                  , Tw.animate_spin
                  ]
                ]
            )
        , style "border-top-color" "transparent"
        ]
        []


errorView : Maybe String -> Html Msg
errorView error =
    case error of
        Just errMsg ->
            div [ css [ Tw.modal ], class "modal-open" ]
                [ div [ css [ Tw.modal_box, Tw.alert, Tw.alert_error ] ]
                    [ text errMsg ]
                ]

        Nothing ->
            text ""


appView : AppData -> Html Msg
appView appData =
    let
        schedules =
            RefreshData.mapTo (\_ -> identity) appData.schedules
    in
    div []
        [ errorView appData.error
        , headerView appData.signedInUser
        , div [ css [ Tw.flex ] ]
            [ streamerListView
                (RefreshData.map
                    (\streamers -> Present (streamersWithSelection appData.selectedStreamers streamers))
                    appData.streamers
                )
                appData.follows
                appData.sidebarStreamerCount
                appData.streamerFilterName

            -- Content placeholder
            , div
                [ css
                    [ Tw.bg_base_100
                    , Tw.h_screen
                    , Tw.w_full
                    , Tw.ml_60
                    , Tw.mt_16
                    ]
                ]
                -- Test fetching streaming schedule
                [ div []
                    (appData.selectedStreamers
                        |> List.map (\streamer -> text (streamer.displayName ++ " "))
                    )
                , button [ css [ Tw.btn, Tw.btn_primary, Css.hover [ Tw.bg_primary_focus ] ], onClick FetchStreamingSchedules ] [ text "Load schedule" ]
                , div [ css [ Tw.text_white ] ]
                    (List.map (\s -> p [ css [ Tw.mt_4 ] ] [ text (Debug.toString s) ]) schedules)
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


streamerListView : RefreshData Http.Error (List ( Twitch.User, Bool )) -> List Twitch.FollowRelation -> Int -> Maybe String -> Html Msg
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
                    button [ linkButtonStyle, onClick (StreamerListMsg ShowMore) ] [ text "Show more" ]

                  else
                    text ""
                , if showCount > streamerListPageSteps then
                    button [ linkButtonStyle, onClick (StreamerListMsg ShowLess) ] [ text "Show less" ]

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
                            div [ css [ Tw.mt_2, Tw.mx_2 ] ] [ text (errorToString error) ]

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
                    , onInput (\s -> StreamerListMsg (Filter s))
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


streamerView : Twitch.User -> Bool -> Html Msg
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
            (StreamerListMsg (SetStreamerSelection streamer (not isSelected)))
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


streamerListPageSteps : Int
streamerListPageSteps =
    10


main : Program Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \urlRequest -> UrlMsg (LinkClicked urlRequest)
        , onUrlChange = \_ -> UrlMsg UrlChanged
        }

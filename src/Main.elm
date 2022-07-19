module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Css
import Css.Global
import Data
import Html.Styled exposing (Html, a, button, div, h1, img, input, label, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (alt, class, css, href, placeholder, src, style, type_)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import RefreshData exposing (RefreshData(..))
import Tailwind.Utilities as Tw
import Twitch
import TwitchConfig
import Url
import Utils


loginRedirectUrl : String
loginRedirectUrl =
    "http://localhost:8000/src/Main.elm"


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
    | GotUserFollows (Result Http.Error (Twitch.PaginatedResponse (List Twitch.FollowRelation)))
    | GotStreamerProfilesForSidebar (Result Http.Error (List Twitch.User))
    | GotStreamerProfiles (Result Http.Error (List Twitch.User))
    | GotUserProfile (Result Http.Error Twitch.User)
    | StreamerListMsg StreamerListMsg


type StreamerListMsg
    = ShowMore
    | ShowLess
    | Filter String
    | SetStreamerSelection Twitch.User Bool


type UrlMsg
    = LinkClicked Browser.UrlRequest
    | UrlChanged


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url navKey =
    case Twitch.accessTokenFromUrl url of
        Just token ->
            ( LoadingScreen
                { token = token
                , follows = Nothing
                , signedInUser = Nothing
                , firstStreamers = Nothing
                }
                navKey
            , Cmd.map GotValidateTokenResponse (Twitch.validateToken token)
            )

        Nothing ->
            ( NotLoggedIn Nothing navKey, Cmd.none )


fetchStreamerProfiles : List String -> Twitch.Token -> Cmd Msg
fetchStreamerProfiles userIDs token =
    Cmd.map GotStreamerProfilesForSidebar (Twitch.getUsers userIDs TwitchConfig.clientId token)


fetchUserProfile : String -> Twitch.Token -> Cmd Msg
fetchUserProfile userID token =
    Cmd.map GotUserProfile (Twitch.getUser userID TwitchConfig.clientId token)


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
                            , fetchUserProfile value.userID m.token
                            )

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
                                    , fetchStreamerProfiles (List.map .toID (List.take streamerListPageSteps oldAndNewValues)) user.token
                                    )

                        ( Nothing, Ok _ ) ->
                            Debug.todo "this case should not happen"

                GotStreamerProfilesForSidebar response ->
                    case ( m.signedInUser, m.follows, Data.fromResult response ) of
                        ( Just user, Just follows, Data.Success streamers ) ->
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
                                }
                                navKey
                            , Cmd.none
                            )

                        ( _, _, Data.Failure e ) ->
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

                StreamerListMsg _ ->
                    ( model, Cmd.none )

                GotStreamerProfiles _ ->
                    ( model, Cmd.none )

        LoggedIn appData navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( LoggedIn appData navKey, handleUrlMsg urlMsg navKey )

                GotValidateTokenResponse _ ->
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
                        , fetchStreamerProfiles nextIDs appData.signedInUser.token
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
                        searchResultswithoutProfile =
                            appData.follows
                                |> filterFollowsByLogin name
                                |> (\f -> missingProfileLogins f (RefreshData.mapTo (\_ v -> v) appData.streamers))
                    in
                    ( LoggedIn { appData | streamerFilterName = Just name } navKey
                    , if String.length name >= 4 && List.length searchResultswithoutProfile > 0 then
                        Cmd.map GotStreamerProfiles (Twitch.getUsers searchResultswithoutProfile TwitchConfig.clientId appData.signedInUser.token)

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

        NotLoggedIn _ navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

                -- this msg should not be relevant for the LoadingScreen model
                GotValidateTokenResponse _ ->
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
                (List.length appData.follows > appData.sidebarStreamerCount)
                appData.streamerFilterName

            -- Content placeholder
            , div
                [ css
                    [ Tw.bg_base_100
                    , Tw.h_screen
                    , Tw.w_full
                    , Tw.ml_60
                    , Tw.flex
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.text_5xl
                    , Tw.font_semibold
                    ]
                ]
                (appData.selectedStreamers
                    |> List.map (\streamer -> text (streamer.displayName ++ " "))
                )
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


filterFollowsByLogin : String -> List Twitch.FollowRelation -> List Twitch.FollowRelation
filterFollowsByLogin name =
    List.filter
        (\f ->
            f.toName
                |> String.toLower
                |> String.contains
                    name
        )



{- Compile a list of all user IDs for streamers that are part of the follows list, but whos profiles are not in the streamers list
   (aka: the details are not fetched yet)
-}


missingProfileLogins : List Twitch.FollowRelation -> List Twitch.User -> List String
missingProfileLogins follows streamers =
    follows
        |> List.filterMap
            (\follow ->
                if List.any (\streamer -> follow.toLogin == streamer.loginName) streamers then
                    Nothing

                else
                    Just follow.toID
            )



{- pair every item of the second list with a bool indicating whether it is part of the selected streamers list -}


streamersWithSelection : List Twitch.User -> List Twitch.User -> List ( Twitch.User, Bool )
streamersWithSelection selected users =
    users
        |> List.map (\u -> ( u, List.any ((==) u) selected ))


streamerListView : RefreshData Http.Error (List ( Twitch.User, Bool )) -> List Twitch.FollowRelation -> Int -> Bool -> Maybe String -> Html Msg
streamerListView streamers follows showCount moreAvailable filterString =
    let
        restStreamersView =
            streamers
                |> RefreshData.mapTo (\_ list -> list)
                |> List.filter (\( _, selected ) -> not selected)
                |> List.take showCount
                |> List.map (\( streamer, isSelected ) -> streamerView streamer isSelected)

        linkButtonStyle =
            css [ Tw.text_primary, Tw.underline ]

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
            if RefreshData.isLoading streamers then
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
                streamers

        filter =
            div []
                [ label
                    [ css [ Tw.label ]
                    ]
                    [ span [ css [ Tw.label_text ] ] [ text "Search" ]
                    ]
                , input
                    [ type_ "text"
                    , placeholder "Channel name"
                    , css [ Tw.input, Tw.input_ghost, Tw.input_bordered ]
                    , onInput (\s -> StreamerListMsg (Filter s))
                    ]
                    []
                ]

        filteredList =
            case filterString of
                Just query ->
                    if String.length query >= 4 then
                        -- this is also computed in update, so we could save us the computation here
                        filterFollowsByLogin query follows
                            |> List.map
                                (\follow ->
                                    let
                                        streamerProfile =
                                            RefreshData.mapTo (\_ v -> v) streamers
                                                |> List.filter (\( user, _ ) -> user.id == follow.toID)
                                                |> List.head
                                    in
                                    case streamerProfile of
                                        Just ( isSelected, streamer ) ->
                                            streamerView isSelected streamer

                                        Nothing ->
                                            loadingSpinner [ Tw.w_8, Tw.h_8 ]
                                )

                    else
                        [ text "enter at least 4 characters" ]

                Nothing ->
                    [ text "" ]

        filterResultsView =
            div [ css [ Tw.border, Tw.border_2 ] ]
                filteredList

        selectedView =
            div []
                (streamers
                    |> RefreshData.mapTo (\_ list -> list)
                    |> List.filter (\( _, selected ) -> selected)
                    |> List.map (\( selected, streamer ) -> streamerView selected streamer)
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
                [ filter
                , div
                    []
                    (List.concat
                        [ [ filterResultsView, selectedView ], restStreamersView, [ buttons, errorText, spinner ] ]
                    )
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


userView : SignedInUser -> Html msg
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
            div [ css [ Tw.avatar ] ]
                [ div [ css [ Tw.rounded_full, Tw.w_10, Tw.h_10 ] ]
                    [ img [ src imgUrl, alt (name ++ " profile image") ] []
                    ]
                ]
    in
    div [ css [ Tw.flex, Tw.items_center ] ]
        [ p [ css [ Tw.mr_2, Tw.font_semibold ] ] [ text name ]
        , avatar
        ]


streamerListPageSteps : Int
streamerListPageSteps =
    10


main : Program () Model Msg
main =
    Browser.application
        { init = \_ url navKey -> init url navKey
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \urlRequest -> UrlMsg (LinkClicked urlRequest)
        , onUrlChange = \_ -> UrlMsg UrlChanged
        }

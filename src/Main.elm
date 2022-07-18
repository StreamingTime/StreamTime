module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Css
import Css.Global
import Data
import Html.Styled exposing (Html, a, button, div, h1, img, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (alt, class, css, href, src, style)
import Html.Styled.Events exposing (onClick)
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
    , streamers : RefreshData (List Twitch.User)

    -- a list of follow relation metadata originating from our user
    , follows : List Twitch.FollowRelation
    , sidebarStreamerCount : Int
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
    | GotStreamerProfiles (Result Http.Error (List Twitch.User))
    | GotUserProfile (Result Http.Error Twitch.User)
    | StreamerListMsg StreamerListMsg


type StreamerListMsg
    = ShowMore
    | ShowLess


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
    Cmd.map GotStreamerProfiles (Twitch.getUsers userIDs TwitchConfig.clientId token)


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

                GotStreamerProfiles response ->
                    case ( m.signedInUser, m.follows, Data.fromResult response ) of
                        ( Just user, Just follows, Data.Success streamers ) ->
                            -- all good, loading is complete
                            ( LoggedIn { signedInUser = user, follows = follows, streamers = Present streamers, sidebarStreamerCount = streamerListPageSteps, error = Nothing } navKey
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

                GotStreamerProfiles response ->
                    case response of
                        Ok newProfiles ->
                            ( LoggedIn
                                { appData
                                    | streamers = RefreshData.map (\oldProfiles -> Present (oldProfiles ++ newProfiles)) appData.streamers
                                    , sidebarStreamerCount = appData.sidebarStreamerCount + streamerListPageSteps
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

                StreamerListMsg streamerListMsg ->
                    let
                        loadMore =
                            List.length appData.follows > RefreshData.mapTo (\_ -> List.length) appData.streamers

                        cmd =
                            case streamerListMsg of
                                ShowMore ->
                                    if loadMore then
                                        let
                                            nextIDs =
                                                appData.follows
                                                    |> List.drop appData.sidebarStreamerCount
                                                    |> List.take streamerListPageSteps
                                                    |> List.map .toID
                                        in
                                        fetchStreamerProfiles nextIDs appData.signedInUser.token

                                    else
                                        Cmd.none

                                ShowLess ->
                                    Cmd.none
                    in
                    if loadMore then
                        ( LoggedIn
                            { appData
                                | streamers = RefreshData.map LoadingMore appData.streamers
                            }
                            navKey
                        , cmd
                        )

                    else
                        let
                            count =
                                case streamerListMsg of
                                    ShowMore ->
                                        min (appData.sidebarStreamerCount + streamerListPageSteps) (List.length appData.follows)

                                    ShowLess ->
                                        max (appData.sidebarStreamerCount - streamerListPageSteps) streamerListPageSteps
                        in
                        ( LoggedIn { appData | sidebarStreamerCount = count } navKey, cmd )

        NotLoggedIn _ navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

                -- this msg should not be relevant for the LoadingScreen model
                GotValidateTokenResponse _ ->
                    ( model, Cmd.none )

                GotUserFollows _ ->
                    ( model, Cmd.none )

                GotStreamerProfiles _ ->
                    ( model, Cmd.none )

                StreamerListMsg _ ->
                    ( model, Cmd.none )

                GotUserProfile _ ->
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
            div [ css [ Tw.modal, Tw.modal ], class "modal-open" ]
                [ div [ css [ Tw.modal_box, Tw.alert, Tw.alert_error ] ]
                    [ text errMsg ]
                ]

        Nothing ->
            text ""


appView : AppData -> Html Msg
appView appData =
    div []
        [ text ("user: " ++ Debug.toString appData.signedInUser)
        , userView appData.signedInUser
        , errorView appData.error
        , div
            []
            [ streamerListView appData.streamers appData.sidebarStreamerCount (List.length appData.follows > appData.sidebarStreamerCount) ]
        ]


streamerListPageSteps : Int
streamerListPageSteps =
    10


streamerListView : RefreshData (List Twitch.User) -> Int -> Bool -> Html Msg
streamerListView streamers showCount moreAvailable =
    let
        linkButtonStyle =
            css [ Tw.btn, Tw.btn_link ]

        listView =
            div []
                (streamers
                    |> RefreshData.mapTo (\_ list -> list)
                    |> List.take showCount
                    |> List.map streamerView
                )

        buttons =
            div []
                [ if moreAvailable then
                    button [ linkButtonStyle, onClick (StreamerListMsg ShowMore) ] [ text "More" ]

                  else
                    text ""
                , --if List.length streamers > streamerListPageSteps then
                  -- TODO: dont show less button all the time
                  button [ linkButtonStyle, onClick (StreamerListMsg ShowLess) ] [ text "Less" ]

                --else
                --text ""
                ]
    in
    div [ style "width" "200px" ]
        [ listView
        , buttons
        , RefreshData.mapTo
            (\err _ ->
                case err of
                    Just error ->
                        text (errorToString error)

                    Nothing ->
                        text ""
            )
            streamers
        , if RefreshData.isLoading streamers then
            loadingSpinner [ Tw.w_8, Tw.h_8 ]

          else
            text ""
        ]


streamerView : Twitch.User -> Html msg
streamerView streamer =
    let
        avatar =
            div [ css [ Tw.avatar ] ]
                [ div [ css [ Tw.rounded_full, Tw.w_10, Tw.h_10 ] ]
                    [ img [ src streamer.profileImageUrl ] []
                    ]
                ]
    in
    a [ href ("https://twitch.tv/" ++ streamer.displayName) ]
        [ div
            [ css
                [ Tw.flex
                , Tw.gap_2
                , Tw.items_center -- center text vertically
                , Tw.m_1
                , Css.hover [ Tw.bg_primary_focus ]
                ]
            ]
            [ avatar
            , div [ css [ Tw.p_1, Tw.font_medium, Tw.truncate ] ] [ text streamer.displayName ]
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
    div []
        [ span []
            [ text name ]
        , avatar
        ]


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

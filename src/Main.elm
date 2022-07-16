module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Css
import Css.Global
import Data exposing (Data)
import Html.Styled exposing (Html, a, button, div, img, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, src, style)
import Html.Styled.Events exposing (onClick)
import Http
import Tailwind.Utilities as Tw
import Twitch
import TwitchConfig
import Url
import Utils


loginRedirectUrl : String
loginRedirectUrl =
    "http://localhost:8000/src/Main.elm"


type Model
    = {- User is logged in, token is verified -} LoggedIn TwitchData Nav.Key
    | {- User has not started the login process -} NotLoggedIn (Maybe Http.Error) Nav.Key
    | {- User has logged in via twitch, but we have yet to validate the token and fetch user details -} PreValidation String Nav.Key


type alias TwitchData =
    { signedInUser : SignedInUser
    , streamers : Data (List Twitch.User)

    -- a list of follow relation metadata originating from our user
    , follows : Maybe (List Twitch.FollowRelation)
    , sidebarStreamerCount : Int
    }


type alias SignedInUser =
    { token : String
    , loginName : String
    , userID : String
    }


type Msg
    = UrlMsg UrlMsg
    | GotValidateTokenResponse (Result Http.Error Twitch.ValidateTokenResponse)
    | GotUserFollows (Result Http.Error (Twitch.PaginatedResponse (List Twitch.FollowRelation)))
    | GotStreamerProfiles (Result Http.Error (List Twitch.User))
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
            ( PreValidation token navKey, Cmd.map GotValidateTokenResponse (Twitch.validateToken token) )

        Nothing ->
            ( NotLoggedIn Nothing navKey, Cmd.none )


fetchStreamerProfiles : List String -> String -> Cmd Msg
fetchStreamerProfiles userIDs token =
    Cmd.map GotStreamerProfiles (Twitch.getUsers userIDs TwitchConfig.clientId token)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        PreValidation token navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

                GotValidateTokenResponse response ->
                    case response of
                        Err err ->
                            ( NotLoggedIn (Just err) navKey, Cmd.none )

                        Ok value ->
                            ( LoggedIn { signedInUser = { token = token, loginName = value.login, userID = value.userID }, sidebarStreamerCount = 10, streamers = Data.Loading, follows = Nothing } navKey
                              -- Fetch streamers our user follows
                            , Cmd.map GotUserFollows (Twitch.getUserFollows value.userID Nothing TwitchConfig.clientId token)
                            )

                GotUserFollows _ ->
                    ( model, Cmd.none )

                GotStreamerProfiles _ ->
                    ( model, Cmd.none )

                StreamerListMsg _ ->
                    ( model, Cmd.none )

        LoggedIn twitchData navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( LoggedIn twitchData navKey, handleUrlMsg urlMsg navKey )

                GotValidateTokenResponse _ ->
                    ( LoggedIn { twitchData | streamers = Data.Loading } navKey, Cmd.map GotUserFollows (Twitch.getUserFollows twitchData.signedInUser.userID Nothing TwitchConfig.clientId twitchData.signedInUser.token) )

                GotUserFollows response ->
                    case response of
                        Err e ->
                            Debug.todo ("error at GotUserFollows " ++ Debug.toString e)

                        Ok paginatedResponse ->
                            let
                                -- append the new page to the axisting values, if any
                                oldAndNewValues =
                                    Utils.concatMaybeList twitchData.follows paginatedResponse.data

                                -- use the cursor from the response to fetch the next page
                                nextPage : String -> Cmd Msg
                                nextPage cursor =
                                    Cmd.map GotUserFollows (Twitch.getUserFollows twitchData.signedInUser.userID (Just cursor) TwitchConfig.clientId twitchData.signedInUser.token)

                                nextCommand =
                                    -- if there is more to load, fetch the next page
                                    case paginatedResponse.cursor of
                                        Nothing ->
                                            fetchStreamerProfiles (List.map .toID (List.take 10 oldAndNewValues)) twitchData.signedInUser.token

                                        Just cursor ->
                                            nextPage cursor
                            in
                            ( LoggedIn { twitchData | follows = Just oldAndNewValues } navKey, nextCommand )

                GotStreamerProfiles response ->
                    case response of
                        Ok newProfiles ->
                            let
                                updatedList =
                                    case twitchData.streamers of
                                        Data.Success oldList ->
                                            oldList ++ newProfiles

                                        _ ->
                                            newProfiles
                            in
                            ( LoggedIn { twitchData | streamers = Data.Success updatedList } navKey, Cmd.none )

                        Err _ ->
                            Debug.todo "error handling"

                StreamerListMsg streamerListMsg ->
                    case ( twitchData.follows, twitchData.streamers ) of
                        ( Just follows, Data.Success streamers ) ->
                            let
                                count =
                                    case streamerListMsg of
                                        ShowMore ->
                                            min (twitchData.sidebarStreamerCount + 10) (List.length follows)

                                        ShowLess ->
                                            max (twitchData.sidebarStreamerCount - 10) 10

                                cmd =
                                    case streamerListMsg of
                                        ShowMore ->
                                            if List.length follows > List.length streamers then
                                                let
                                                    nextIDs =
                                                        follows
                                                            |> List.drop twitchData.sidebarStreamerCount
                                                            |> List.take 10
                                                            |> List.map .toID
                                                in
                                                fetchStreamerProfiles nextIDs twitchData.signedInUser.token

                                            else
                                                Cmd.none

                                        ShowLess ->
                                            Cmd.none
                            in
                            ( LoggedIn { twitchData | sidebarStreamerCount = count } navKey, cmd )

                        _ ->
                            Debug.todo "error handling"

        NotLoggedIn _ navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

                -- this msg should not be relevant in NotLoffedIn state
                GotValidateTokenResponse _ ->
                    ( model, Cmd.none )

                GotUserFollows _ ->
                    ( model, Cmd.none )

                GotStreamerProfiles _ ->
                    ( model, Cmd.none )

                StreamerListMsg _ ->
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

                        PreValidation _ _ ->
                            validationView

                        LoggedIn appData _ ->
                            appView appData
                    ]
                ]
        ]
    }


loginView : Maybe Http.Error -> Html Msg
loginView err =
    div []
        [ a
            [ href (Twitch.loginFlowUrl TwitchConfig.clientId loginRedirectUrl) ]
            [ text "Login" ]
        , case err of
            Nothing ->
                text ""

            Just e ->
                text (errorToString e)
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
            [ css
                []
            ]
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


appView : TwitchData -> Html Msg
appView appData =
    div []
        [ text ("user: " ++ Debug.toString appData.signedInUser)
        , div
            []
            (case ( appData.follows, appData.streamers ) of
                ( Just follows, Data.Success streamers ) ->
                    [ text ("Following " ++ String.fromInt (List.length follows) ++ " streamers")
                    , streamerListView (List.take appData.sidebarStreamerCount streamers) (List.length follows > appData.sidebarStreamerCount)
                    ]

                ( Just follows, Data.Loading ) ->
                    [ text ("Following " ++ String.fromInt (List.length follows) ++ " streamers")
                    , loadingSpinner
                        [ Tw.h_14, Tw.w_14 ]
                    ]

                ( Just follows, Data.Failure e ) ->
                    [ text ("Following " ++ String.fromInt (List.length follows) ++ " streamers") ]

                _ ->
                    [ text "error" ]
            )
        ]


streamerListView : List Twitch.User -> Bool -> Html Msg
streamerListView streamers moreAvailable =
    let
        linkButtonStyle =
            css [ Tw.btn, Tw.btn_link ]
    in
    div [ style "width" "200px" ]
        [ div []
            [ text ("show: " ++ String.fromInt (List.length streamers))
            , div []
                (List.map streamerView streamers)
            , div []
                [ if moreAvailable then
                    button [ linkButtonStyle, onClick (StreamerListMsg ShowMore) ] [ text "More" ]

                  else
                    text ""
                , if List.length streamers > 10 then
                    button [ linkButtonStyle, onClick (StreamerListMsg ShowLess) ] [ text "Less" ]

                  else
                    text ""
                ]
            ]
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

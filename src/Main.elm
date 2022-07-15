module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Css.Global
import Data exposing (Data(..))
import Html.Styled as Html exposing (Html, a, div, text, toUnstyled)
import Html.Styled.Attributes exposing (href)
import Http
import Tailwind.Utilities as Tw
import Twitch
import TwitchConfig
import Url


loginRedirectUrl =
    "http://localhost:8000/src/Main.elm"


type Model
    = {- User is logged in, token is verified -} LoggedIn SignedInUser (Data (List Twitch.User)) Nav.Key
    | {- User has not started the login process -} NotLoggedIn (Maybe Http.Error) Nav.Key
    | {- User has logged in via twitch, but we have yet to validate the token and fetch user details -} PreValidation String Nav.Key


type alias SignedInUser =
    { token : String
    , loginName : String
    , userID : String
    }


type Msg
    = UrlMsg UrlMsg
    | GotValidateTokenResponse (Result Http.Error Twitch.ValidateTokenResponse)
    | GotUserFollows (Result Http.Error (List Twitch.FollowRelation))
    | GotFollowedStreamers (Result Http.Error (List Twitch.User))


type UrlMsg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url navKey =
    case Twitch.accessTokenFromUrl url of
        Just token ->
            ( PreValidation token navKey, Cmd.map GotValidateTokenResponse (Twitch.validateToken token) )

        Nothing ->
            ( NotLoggedIn Nothing navKey, Cmd.none )


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
                            ( LoggedIn { token = token, loginName = value.login, userID = value.userID } Data.Loading navKey
                              -- Fetch streamers our user follows
                            , Cmd.map GotUserFollows (Twitch.getUserFollows value.userID TwitchConfig.clientId token)
                            )

                GotUserFollows _ ->
                    ( model, Cmd.none )

                GotFollowedStreamers _ ->
                    ( model, Cmd.none )

        LoggedIn user _ navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( LoggedIn user Data.Loading navKey, handleUrlMsg urlMsg navKey )

                GotValidateTokenResponse _ ->
                    ( LoggedIn user Data.Loading navKey, Cmd.map GotUserFollows (Twitch.getUserFollows user.userID TwitchConfig.clientId user.token) )

                GotUserFollows response ->
                    case response of
                        Err e ->
                            Debug.todo ("error at GotUserFollows " ++ Debug.toString e)

                        Ok value ->
                            let
                                followingIDs =
                                    List.map (\followRelation -> followRelation.toID) value
                            in
                            ( model, Cmd.map GotFollowedStreamers (Twitch.getUsers followingIDs TwitchConfig.clientId user.token) )

                GotFollowedStreamers response ->
                    ( LoggedIn user (Data.fromResult response) navKey, Cmd.none )

        NotLoggedIn _ navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

                -- this msg should not be relevant in NotLoffedIn state
                GotValidateTokenResponse _ ->
                    ( model, Cmd.none )

                GotUserFollows _ ->
                    ( model, Cmd.none )

                GotFollowedStreamers _ ->
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

        UrlChanged _ ->
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
                , Html.div []
                    [ case model of
                        NotLoggedIn err _ ->
                            loginView err

                        PreValidation _ _ ->
                            validationView

                        LoggedIn user streamers _ ->
                            appView user streamers
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
    div [] [ text "Loading..." ]


appView : SignedInUser -> Data (List Twitch.User) -> Html Msg
appView user streamers =
    div []
        [ text ("user: " ++ Debug.toString user)
        , div [] [ text ("streamers: " ++ Debug.toString streamers) ]
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = \_ url navKey -> init url navKey
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \urlRequest -> UrlMsg (LinkClicked urlRequest)
        , onUrlChange = \url -> UrlMsg (UrlChanged url)
        }

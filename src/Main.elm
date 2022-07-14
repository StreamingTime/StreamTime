module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (a, text)
import Html.Attributes exposing (href)
import Http
import Twitch
import TwitchConfig
import Url


loginRedirectUrl =
    "http://localhost:8000/src/Main.elm"


type Model
    = {- User is logged in, token is verified -} LoggedIn User Nav.Key
    | {- User has not started the login process -} NotLoggedIn Nav.Key
    | {- User has logged in via twitch, but we have yet to validate the token and fetch user details -} PreValidation String Nav.Key


type alias User =
    { token : String
    , loginName : String
    , userID : String
    }


type Msg
    = UrlMsg UrlMsg
    | GotValidateTokenResponse (Result Http.Error Twitch.ValidateTokenResponse)


type UrlMsg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url navKey =
    case Twitch.accessTokenFromUrl url of
        Just token ->
            ( PreValidation token navKey, Cmd.map GotValidateTokenResponse (Twitch.validateToken token) )

        Nothing ->
            ( NotLoggedIn navKey, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        PreValidation token navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

                GotValidateTokenResponse response ->
                    case response of
                        Err _ ->
                            Debug.todo "error handling"

                        Ok value ->
                            ( LoggedIn { token = token, loginName = value.login, userID = value.userID } navKey, Cmd.none )

        LoggedIn user navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( LoggedIn user navKey, handleUrlMsg urlMsg navKey )

                GotValidateTokenResponse _ ->
                    ( LoggedIn user navKey, Cmd.none )

        NotLoggedIn navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

                -- this msg should not be relevant in NotLoffedIn state
                GotValidateTokenResponse _ ->
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


view : Model -> Document Msg
view model =
    { title = "Twitch schedule"
    , body =
        case model of
            LoggedIn user _ ->
                [ text ("user: " ++ Debug.toString user)
                ]

            NotLoggedIn _ ->
                [ a
                    [ href (Twitch.loginFlowUrl TwitchConfig.clientId loginRedirectUrl) ]
                    [ text "Login" ]
                ]

            PreValidation _ _ ->
                [ text "verifying twitch token" ]
    }


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

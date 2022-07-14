module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (a, text)
import Html.Attributes exposing (href)
import Twitch
import TwitchConfig
import Url


loginRedirectUrl =
    "http://localhost:8000/src/Main.elm"


type Model
    = LoggedIn User Nav.Key
    | NotLoggedIn Nav.Key


type alias User =
    { token : String }


type Msg
    = UrlMsg UrlMsg


type UrlMsg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


init : Url.Url -> Nav.Key -> ( Model, Cmd msg )
init url navKey =
    case Twitch.accessTokenFromUrl url of
        Just token ->
            ( LoggedIn { token = token } navKey, Cmd.none )

        Nothing ->
            ( NotLoggedIn navKey, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        LoggedIn _ navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )

        NotLoggedIn navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( model, handleUrlMsg urlMsg navKey )


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


view : Model -> Document msg
view model =
    { title = "Twitch schedule"
    , body =
        case model of
            LoggedIn user _ ->
                [ text user.token ]

            NotLoggedIn _ ->
                [ a
                    [ href (Twitch.loginFlowUrl TwitchConfig.clientId loginRedirectUrl) ]
                    [ text "Login" ]
                ]
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

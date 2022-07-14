module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (a, div, text)
import Html.Attributes exposing (href)
import Twitch
import TwitchConfig
import Url


loginRedirectUrl =
    "http://localhost:8000/src/Main.elm"


type alias Model =
    { token : String
    , navKey : Nav.Key
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


init : Nav.Key -> ( Model, Cmd msg )
init navKey =
    ( { token = "no token yet"
      , navKey = navKey
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( model
            , Cmd.none
            )


view : Model -> Document msg
view model =
    { title = "Twitch schedule"
    , body =
        [ div []
            [ text model.token
            , a
                [ href (Twitch.loginFlowUrl TwitchConfig.clientId loginRedirectUrl) ]
                [ text "Login" ]
            ]
        ]
    }


main : Program () Model Msg
main =
    Browser.application
        { init = \_ _ navKey -> init navKey
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

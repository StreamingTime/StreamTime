module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Css
import Css.Global
import Html.Styled exposing (Html, a, button, div, h1, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, style)
import Http
import Tailwind.Utilities as Tw
import Twitch
import TwitchConfig
import Url


loginRedirectUrl : String
loginRedirectUrl =
    "http://localhost:8000/src/Main.elm"


type Model
    = {- User is logged in, token is verified -} LoggedIn User Nav.Key
    | {- User has not started the login process -} NotLoggedIn (Maybe Http.Error) Nav.Key
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
    | UrlChanged


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
                            ( LoggedIn { token = token, loginName = value.login, userID = value.userID } navKey, Cmd.none )

        LoggedIn user navKey ->
            case msg of
                UrlMsg urlMsg ->
                    ( LoggedIn user navKey, handleUrlMsg urlMsg navKey )

                GotValidateTokenResponse _ ->
                    ( LoggedIn user navKey, Cmd.none )

        NotLoggedIn _ navKey ->
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

                        LoggedIn user _ ->
                            appView user
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
                , button
                    [ css
                        [ Tw.btn
                        , Tw.btn_primary
                        ]
                    ]
                    [ a
                        [ href (Twitch.loginFlowUrl TwitchConfig.clientId loginRedirectUrl) ]
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


appView : User -> Html Msg
appView user =
    div []
        [ text ("user: " ++ Debug.toString user)
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

module Twitch exposing (ValidateTokenResponse, accessTokenFromUrl, decodeValidateTokenResponse, loginFlowUrl, validateToken)

import Http
import Json.Decode as Decode
import Maybe exposing (andThen)
import Url
import Url.Builder


type alias ValidateTokenResponse =
    { clientID : String
    , login : String
    , userID : String
    }


validateToken : String -> Cmd (Result Http.Error ValidateTokenResponse)
validateToken =
    oAuthRequest "https://id.twitch.tv/oauth2/validate" decodeValidateTokenResponse


decodeValidateTokenResponse : Decode.Decoder ValidateTokenResponse
decodeValidateTokenResponse =
    Decode.map3 ValidateTokenResponse
        (Decode.field "client_id" Decode.string)
        (Decode.field "login" Decode.string)
        (Decode.field "user_id" Decode.string)



{- create an HTTP.request with OAuth header -}


oAuthRequest : String -> Decode.Decoder a -> String -> Cmd (Result Http.Error a)
oAuthRequest url decoder token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("OAuth " ++ token) ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson identity decoder
        , timeout = Nothing
        , tracker = Nothing
        }



{-
   Construct a URL for the Twitch "Implicit grant flow" authentication method
-}


loginFlowUrl : String -> String -> String
loginFlowUrl clientID redirectUri =
    Url.Builder.crossOrigin "https://id.twitch.tv"
        [ "oauth2", "authorize" ]
        [ Url.Builder.string "client_id" clientID
        , Url.Builder.string "redirect_uri" redirectUri
        , Url.Builder.string "response_type" "token"
        ]


accessTokenFromUrl : Url.Url -> Maybe String
accessTokenFromUrl url =
    url.fragment |> andThen getAccessToken


getAccessToken : String -> Maybe String
getAccessToken fragment =
    case List.filter (\( x, _ ) -> x == "access_token") (parseFragmentValues fragment) of
        [] ->
            Nothing

        ( _, y ) :: _ ->
            Just y


parseFragmentValues : String -> List ( String, String )
parseFragmentValues url =
    List.filterMap toKeyValue (String.split "&" url)


toKeyValue : String -> Maybe ( String, String )
toKeyValue item =
    case String.split "=" item of
        [ key, value ] ->
            Just ( key, value )

        _ ->
            Nothing

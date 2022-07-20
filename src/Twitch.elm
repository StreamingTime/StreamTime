module Twitch exposing (ClientID(..), FollowRelation, PaginatedResponse, Token(..), User, ValidateTokenResponse, accessTokenFromUrl, decodeFollowRelation, decodeListHead, decodePaginated, decodeUser, decodeValidateTokenResponse, getTokenValue, getUser, getUserFollows, getUsers, loginFlowUrl, validateToken)

import Http
import Json.Decode as Decode
import Maybe exposing (andThen)
import Url
import Url.Builder



{- Twitch user access token -}


type Token
    = Token String



{- Twitch client id -}


type ClientID
    = ClientID String


getTokenValue : Token -> String
getTokenValue (Token value) =
    value



{- Url.Builder to construct Twitch Helix urls -}


apiUrlBuilder : List String -> List Url.Builder.QueryParameter -> String
apiUrlBuilder =
    Url.Builder.crossOrigin "https://api.twitch.tv/helix"



-- validate a users token


type alias ValidateTokenResponse =
    { clientID : String
    , login : String
    , userID : String
    }


validateToken : Token -> Cmd (Result Http.Error ValidateTokenResponse)
validateToken =
    oAuthRequest "https://id.twitch.tv/oauth2/validate" decodeValidateTokenResponse


decodeValidateTokenResponse : Decode.Decoder ValidateTokenResponse
decodeValidateTokenResponse =
    Decode.map3 ValidateTokenResponse
        (Decode.field "client_id" Decode.string)
        (Decode.field "login" Decode.string)
        (Decode.field "user_id" Decode.string)



-- get a users follows


type alias FollowRelation =
    { fromID : String
    , fromLogin : String
    , fromName : String
    , toID : String
    , toName : String
    , followedAt : String
    }


decodeFollowRelation : Decode.Decoder FollowRelation
decodeFollowRelation =
    Decode.map6 FollowRelation
        (Decode.field "from_id" Decode.string)
        (Decode.field "from_login" Decode.string)
        (Decode.field "from_name" Decode.string)
        (Decode.field "to_id" Decode.string)
        (Decode.field "to_name" Decode.string)
        (Decode.field "followed_at" Decode.string)



{-
   https://dev.twitch.tv/docs/api/reference#get-users-follows

   Takes an optional cursor to fetch paginated results
-}


getUserFollows : String -> Maybe String -> ClientID -> Token -> Cmd (Result Http.Error (PaginatedResponse (List FollowRelation)))
getUserFollows userID cursor =
    let
        params =
            [ Url.Builder.string "from_id" userID

            -- fetch up to 100 users per page (yes, this is the "first" parameter)
            , Url.Builder.int "first" 100
            ]

        u =
            apiUrlBuilder
                [ "users", "follows" ]
                (case cursor of
                    Just c ->
                        -- pagination
                        Url.Builder.string "after" c :: params

                    Nothing ->
                        params
                )
    in
    bearerRequest u (decodePaginated (Decode.list decodeFollowRelation))



-- get twitch users


type alias User =
    { id : String
    , displayName : String
    , profileImageUrl : String
    }


decodeUser : Decode.Decoder User
decodeUser =
    Decode.map3 User
        (Decode.field "id" Decode.string)
        (Decode.field "display_name" Decode.string)
        (Decode.field "profile_image_url" Decode.string)



{- https://dev.twitch.tv/docs/api/reference#get-users -}


getUsers : List String -> ClientID -> Token -> Cmd (Result Http.Error (List User))
getUsers userIDs =
    let
        u =
            apiUrlBuilder
                [ "users" ]
                (List.map
                    (Url.Builder.string "id")
                    userIDs
                )
    in
    bearerRequest u (Decode.field "data" (Decode.list decodeUser))



{- https://dev.twitch.tv/docs/api/reference#get-users -}


getUser : String -> ClientID -> Token -> Cmd (Result Http.Error User)
getUser userID =
    let
        u =
            apiUrlBuilder
                [ "users" ]
                [ Url.Builder.string "id" userID ]
    in
    bearerRequest u
        (Decode.field "data"
            (decodeListHead decodeUser)
        )


decodeListHead : Decode.Decoder a -> Decode.Decoder a
decodeListHead listItemDecoder =
    let
        headOrFail : List a -> Decode.Decoder a
        headOrFail list =
            case List.head list of
                Just item ->
                    Decode.succeed item

                Nothing ->
                    Decode.fail "Can't take head of empty list"
    in
    Decode.andThen headOrFail (Decode.list listItemDecoder)



{- create an HTTP.request with OAuth header -}


oAuthRequest : String -> Decode.Decoder a -> Token -> Cmd (Result Http.Error a)
oAuthRequest url decoder (Token token) =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("OAuth " ++ token) ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson identity decoder
        , timeout = Nothing
        , tracker = Nothing
        }



{- create an HTTP.request with Bearer token and client id header -}


bearerRequest : String -> Decode.Decoder a -> ClientID -> Token -> Cmd (Result Http.Error a)
bearerRequest url decoder (ClientID clientID) (Token token) =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ token)
            , Http.header "Client-Id" clientID
            ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson identity decoder
        , timeout = Nothing
        , tracker = Nothing
        }



{- Wrapper to fetch paginated resources -}


type alias PaginatedResponse a =
    { cursor : Maybe String
    , data : a
    }



{- Decode a paginated resource. Uses the passed decoder to decode the "data" field -}


decodePaginated : Decode.Decoder a -> Decode.Decoder (PaginatedResponse a)
decodePaginated dataDecoder =
    Decode.map2 PaginatedResponse
        (Decode.field "pagination" (Decode.maybe (Decode.field "cursor" Decode.string)))
        (Decode.field "data" dataDecoder)



{-
   Construct a URL for the Twitch "Implicit grant flow" authentication method
-}


loginFlowUrl : ClientID -> String -> String
loginFlowUrl (ClientID clientID) redirectUri =
    Url.Builder.crossOrigin "https://id.twitch.tv"
        [ "oauth2", "authorize" ]
        [ Url.Builder.string "client_id" clientID
        , Url.Builder.string "redirect_uri" redirectUri
        , Url.Builder.string "response_type" "token"
        ]


accessTokenFromUrl : Url.Url -> Maybe Token
accessTokenFromUrl url =
    url.fragment |> andThen getAccessToken


getAccessToken : String -> Maybe Token
getAccessToken fragment =
    case List.filter (\( x, _ ) -> x == "access_token") (parseFragmentValues fragment) of
        [] ->
            Nothing

        ( _, y ) :: _ ->
            Just (Token y)


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

module Twitch exposing (Category, ClientID(..), FollowRelation, PaginatedResponse, Schedule, Segment, Token(..), User, UserID(..), ValidateTokenResponse, Video, VideoType(..), accessTokenFromUrl, allPages, boxArtUrl, decodeCategory, decodeFollowRelation, decodeListHead, decodePaginated, decodeSchedule, decodeSegment, decodeUser, decodeUserID, decodeValidateTokenResponse, fetchVideos, getLoggedInUserTask, getStreamingSchedule, getTokenValue, getUserFollowsTask, getUsers, getUsersTask, loginFlowUrl, pagesWhile, revokeToken, toFormData, userProfileUrl, validateToken, validateTokenTask, videoPreview)

import Error exposing (Error(..))
import FormatTime
import Http
import Json.Decode as Decode
import Maybe exposing (andThen)
import RFC3339
import Task
import Time
import Url
import Url.Builder



{- Twitch user access token -}


type Token
    = Token String



{- Twitch client id -}


type ClientID
    = ClientID String


type UserID
    = UserID String


decodeUserID : Decode.Decoder UserID
decodeUserID =
    Decode.map UserID Decode.string


userIDtoString : UserID -> String
userIDtoString (UserID s) =
    s


getTokenValue : Token -> String
getTokenValue (Token value) =
    value



{- Url.Builder to construct Twitch Helix urls -}


apiUrlBuilder : List String -> List Url.Builder.QueryParameter -> String
apiUrlBuilder =
    Url.Builder.crossOrigin "https://api.twitch.tv/helix"


{-| Revoke a users token

<https://dev.twitch.tv/docs/authentication/revoke-tokens>

-}
revokeToken : ClientID -> Token -> Cmd (Result Http.Error ())
revokeToken (ClientID clientId) (Token token) =
    let
        data =
            toFormData
                [ ( "client_id", clientId )
                , ( "token", token )
                ]

        body =
            Http.stringBody "application/x-www-form-urlencoded" data
    in
    postRequest "https://id.twitch.tv/oauth2/revoke" body



-- validate a users token


type alias ValidateTokenResponse =
    { clientID : String
    , login : String
    , userID : UserID
    }


{-| <https://dev.twitch.tv/docs/authentication/validate-tokens>
-}
validateToken : Token -> Cmd (Result Http.Error ValidateTokenResponse)
validateToken token =
    Task.attempt identity (validateTokenTask token)


{-| <https://dev.twitch.tv/docs/authentication/validate-tokens>
-}
validateTokenTask : Token -> Task.Task Http.Error ValidateTokenResponse
validateTokenTask (Token token) =
    Http.task
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("OAuth " ++ token) ]
        , url = "https://id.twitch.tv/oauth2/validate"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| decodeValidateTokenResponse
        , timeout = Nothing
        }


decodeValidateTokenResponse : Decode.Decoder ValidateTokenResponse
decodeValidateTokenResponse =
    Decode.map3 ValidateTokenResponse
        (Decode.field "client_id" Decode.string)
        (Decode.field "login" Decode.string)
        (Decode.field "user_id" decodeUserID)



-- get channel stream schedule


type alias Schedule =
    { segments : List Segment
    , broadcasterId : UserID
    , broadcasterName : String
    }



{- Scheduled broadcasts are defined as “stream segments” in the API. -}


type alias Segment =
    { startTime : Time.Posix
    , endTime : Maybe Time.Posix
    , title : String
    , canceledUntil : Maybe Time.Posix
    , category : Maybe Category
    , isRecurring : Bool
    }


type alias Category =
    { name : String
    , id : String
    }


decodeCategory : Decode.Decoder Category
decodeCategory =
    Decode.map2 Category
        (Decode.field "name" Decode.string)
        (Decode.field "id" Decode.string)


boxArtUrl : Category -> Int -> Int -> String
boxArtUrl { id } width height =
    "https://static-cdn.jtvnw.net/ttv-boxart/" ++ id ++ "-" ++ String.fromInt width ++ "x" ++ String.fromInt height ++ ".jpg"


decodeSegment : Decode.Decoder Segment
decodeSegment =
    Decode.map6 Segment
        (Decode.field "start_time" RFC3339.decode)
        (Decode.maybe (Decode.field "end_time" RFC3339.decode))
        (Decode.field "title" Decode.string)
        (Decode.maybe (Decode.field "canceled_until" RFC3339.decode))
        (Decode.maybe (Decode.field "category" decodeCategory))
        (Decode.field "is_recurring" Decode.bool)


decodeSchedule : Decode.Decoder Schedule
decodeSchedule =
    Decode.map3 Schedule
        (Decode.field "segments" (Decode.list decodeSegment))
        (Decode.field "broadcaster_id" decodeUserID)
        (Decode.field "broadcaster_name" Decode.string)



{- https://dev.twitch.tv/docs/api/reference#get-channel-stream-schedule -}


getStreamingSchedule : UserID -> Maybe Time.Posix -> Maybe String -> ClientID -> Token -> Task.Task Error (PaginatedResponse Schedule)
getStreamingSchedule (UserID userID) startTime cursor (ClientID clientID) (Token token) =
    let
        params =
            [ Url.Builder.string "broadcaster_id" userID
            , Url.Builder.int "first" 25
            ]
                ++ (case cursor of
                        Just c ->
                            [ Url.Builder.string "after" c ]

                        Nothing ->
                            []
                   )

        requestUrlWithoutParams =
            apiUrlBuilder [ "schedule" ]

        request =
            { method = "GET"
            , headers =
                [ Http.header "Authorization" ("Bearer " ++ token)
                , Http.header "Client-Id" clientID
                ]
            , url = requestUrlWithoutParams params
            , body = Http.emptyBody
            , resolver = Http.stringResolver <| handleJsonResponse <| decodePaginated decodeSchedule
            , timeout = Nothing
            }
    in
    case startTime of
        Nothing ->
            Task.mapError HttpError (Http.task request)

        Just time ->
            case FormatTime.asRFC3339 Time.utc time of
                Ok t ->
                    Task.mapError HttpError (Http.task { request | url = requestUrlWithoutParams (params ++ [ Url.Builder.string "start_time" t ]) })

                Err _ ->
                    Task.fail (StringError "failed to format start time")



{- https://korban.net/posts/elm/2019-02-15-combining-http-requests-with-task-in-elm/ -}


handleJsonResponse : Decode.Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result



-- get a users follows


type alias FollowRelation =
    { fromID : UserID
    , fromLogin : String
    , fromName : String
    , toID : UserID
    , toName : String
    , toLogin : String
    , followedAt : String
    }


decodeFollowRelation : Decode.Decoder FollowRelation
decodeFollowRelation =
    Decode.map7 FollowRelation
        (Decode.field "from_id" decodeUserID)
        (Decode.field "from_login" Decode.string)
        (Decode.field "from_name" Decode.string)
        (Decode.field "to_id" decodeUserID)
        (Decode.field "to_name" Decode.string)
        (Decode.field "to_login" Decode.string)
        (Decode.field "followed_at" Decode.string)


{-| <https://dev.twitch.tv/docs/api/reference#get-users-follows>

Takes an optional cursor to fetch paginated results

-}
getUserFollowsTask : UserID -> Maybe String -> ClientID -> Token -> Task.Task Http.Error (PaginatedResponse (List FollowRelation))
getUserFollowsTask (UserID userID) cursor =
    let
        params =
            [ Url.Builder.string "user_id" userID

            -- fetch up to 100 users per page (yes, this is the "first" parameter)
            , Url.Builder.int "first" 100
            ]

        u =
            apiUrlBuilder
                [ "channels", "followed" ]
                (case cursor of
                    Just c ->
                        -- pagination
                        Url.Builder.string "after" c :: params

                    Nothing ->
                        params
                )
    in
    bearerGetRequestTask
        u
        (decodePaginated (Decode.list decodeFollowRelation))



-- get twitch users


type alias User =
    { id : UserID
    , displayName : String
    , profileImageUrl : String
    , loginName : String
    }


decodeUser : Decode.Decoder User
decodeUser =
    Decode.map4 User
        (Decode.field "id" decodeUserID)
        (Decode.field "display_name" Decode.string)
        (Decode.field "profile_image_url" Decode.string)
        (Decode.field "login" Decode.string)


{-| <https://dev.twitch.tv/docs/api/reference#get-users>
-}
getUsers : List UserID -> ClientID -> Token -> Cmd (Result Http.Error (List User))
getUsers userIDs clientID token =
    Task.attempt identity (getUsersTask userIDs clientID token)


getUsersTask : List UserID -> ClientID -> Token -> Task.Task Http.Error (List User)
getUsersTask userIDs =
    let
        u =
            apiUrlBuilder
                [ "users" ]
                (userIDs
                    |> List.map userIDtoString
                    |> List.map (Url.Builder.string "id")
                )
    in
    bearerGetRequestTask u (Decode.field "data" (Decode.list decodeUser))


{-| <https://dev.twitch.tv/docs/api/reference#get-users>
-}
getLoggedInUserTask : ClientID -> Token -> Task.Task Http.Error User
getLoggedInUserTask =
    bearerGetRequestTask
        (apiUrlBuilder [ "users" ] [])
        (Decode.field "data" (decodeListHead decodeUser))


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


{-| create an HTTP.request with method post, ignore response body
-}
postRequest : String -> Http.Body -> Cmd (Result Http.Error ())
postRequest url body =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectWhatever identity
        , timeout = Nothing
        , tracker = Nothing
        }


{-| create an HTTP.request with Bearer token and client id header
-}
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


bearerGetRequestTask : String -> Decode.Decoder a -> ClientID -> Token -> Task.Task Http.Error a
bearerGetRequestTask url decoder (ClientID clientID) (Token token) =
    Http.task
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ token)
            , Http.header "Client-Id" clientID
            ]
        , url = url
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| decoder
        , timeout = Nothing
        }



{- get videos -}


type VideoType
    = Upload
    | Highlight
    | Archive


decodeVideoType : Decode.Decoder VideoType
decodeVideoType =
    Decode.andThen
        (\s ->
            case s of
                "upload" ->
                    Decode.succeed Upload

                "archive" ->
                    Decode.succeed Archive

                "highlight" ->
                    Decode.succeed Highlight

                _ ->
                    Decode.fail "unknown video type"
        )
        Decode.string


type alias Video =
    { id : String
    , userID : UserID
    , userLogin : String
    , userName : String
    , title : String
    , description : String
    , createdAt : Time.Posix
    , publishedAt : Time.Posix
    , url : String
    , thumbnailURL : String
    , viewable : String
    , viewCount : Int
    , language : String
    , duration : String
    , videoType : VideoType
    }


decodeVideo : Decode.Decoder Video
decodeVideo =
    Decode.succeed Video
        |> apply (Decode.field "id" Decode.string)
        |> apply (Decode.field "user_id" decodeUserID)
        |> apply (Decode.field "user_login" Decode.string)
        |> apply (Decode.field "user_name" Decode.string)
        |> apply (Decode.field "title" Decode.string)
        |> apply (Decode.field "description" Decode.string)
        |> apply (Decode.field "created_at" RFC3339.decode)
        |> apply (Decode.field "published_at" RFC3339.decode)
        |> apply (Decode.field "url" Decode.string)
        |> apply (Decode.field "thumbnail_url" Decode.string)
        |> apply (Decode.field "viewable" Decode.string)
        |> apply (Decode.field "view_count" Decode.int)
        |> apply (Decode.field "language" Decode.string)
        |> apply (Decode.field "duration" Decode.string)
        |> apply (Decode.field "type" decodeVideoType)


{-| <https://dev.twitch.tv/docs/api/reference#get-videos>
-}
fetchVideos : Int -> UserID -> ClientID -> Token -> Cmd (Result Error (List Video))
fetchVideos count (UserID userID) clientID token =
    let
        u =
            apiUrlBuilder
                [ "videos" ]
                [ Url.Builder.string "user_id" userID
                , Url.Builder.int "first" count
                ]
    in
    Cmd.map (Result.mapError Error.HttpError) (bearerRequest u (Decode.field "data" (Decode.list decodeVideo)) clientID token)


{-| replace the width and height placeholder in a video preview url with the given dimensions
-}
videoPreview : Int -> Int -> String -> String
videoPreview width height =
    String.replace "%{width}x%{height}" (String.fromInt width ++ "x" ++ String.fromInt height)


{-| Wrapper to fetch paginated resources
-}
type alias PaginatedResponse a =
    { cursor : Maybe String
    , data : a
    }


{-| Decode a paginated resource. Uses the passed decoder to decode the "data" field
-}
decodePaginated : Decode.Decoder a -> Decode.Decoder (PaginatedResponse a)
decodePaginated dataDecoder =
    Decode.map2 PaginatedResponse
        (Decode.field "pagination" (Decode.maybe (Decode.field "cursor" Decode.string)))
        (Decode.field "data" dataDecoder)


{-| Fetch all pages of a PaginatedResponse. Calls fetchFromCursor withe new cursor values (or Nothing, for the first page)
-}
allPages : (Maybe String -> Task.Task x (PaginatedResponse (List a))) -> Task.Task x (List a)
allPages fetchFromCursor =
    fetchFromCursor Nothing
        |> Task.andThen (pagesWhile fetchFromCursor (\_ -> True) [])
        |> Task.map List.concat


{-| Recursively fetch a pages using fetchFrom with the new cursor value and collect results into the accumulator

  - _fetchFrom_ takes an optional cursor and fetches a pages starting from there

  - _pred_ takes the data of a fetched page and decides if more pages are to be fetched

  - _acc_ is the accumulator

  - _prevPage_ is the previously fetched page

-}
pagesWhile : (Maybe String -> Task.Task x (PaginatedResponse a)) -> (a -> Bool) -> List a -> PaginatedResponse a -> Task.Task x (List a)
pagesWhile fetchFrom pred accumulator prevPage =
    if pred prevPage.data then
        case prevPage.cursor of
            Just _ ->
                fetchFrom prevPage.cursor
                    |> Task.andThen (pagesWhile fetchFrom pred (accumulator ++ [ prevPage.data ]))

            Nothing ->
                Task.succeed (accumulator ++ [ prevPage.data ])

    else
        Task.succeed (accumulator ++ [ prevPage.data ])


{-| Construct a URL for the Twitch "Implicit grant flow" authentication method
-}
loginFlowUrl : ClientID -> String -> String
loginFlowUrl (ClientID clientID) redirectUri =
    Url.Builder.crossOrigin "https://id.twitch.tv"
        [ "oauth2", "authorize" ]
        [ Url.Builder.string "client_id" clientID
        , Url.Builder.string "scope" "user:read:follows"
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


toFormData : List ( String, String ) -> String
toFormData =
    List.map (\( k, v ) -> Url.percentEncode k ++ "=" ++ Url.percentEncode v)
        >> String.join "&"


apply : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
apply =
    Decode.map2 (|>)


userProfileUrl : String -> String
userProfileUrl userName =
    Url.Builder.crossOrigin "https://twitch.tv" [ userName ] []

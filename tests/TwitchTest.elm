module TwitchTest exposing (accessTokenFromUrlTest, decodeCategoryTest, decodeFollowRelationTest, decodeListHeadTest, decodePaginatedTest, decodeScheduleTest, decodeSegmentTest, decodeUserTest, decodeValidateTokenResponseTest)

import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)
import Twitch
import Url


decodeValidateTokenResponseTest : Test
decodeValidateTokenResponseTest =
    test "decode token validation response"
        (\_ ->
            Expect.equal
                (Result.Ok
                    { clientID = "wbmytr93xzw8zbg0p1izqyzzc5mbiz", login = "twitchdev", userID = "141981764" }
                )
                (Decode.decodeString
                    Twitch.decodeValidateTokenResponse
                    "{\n  \"client_id\": \"wbmytr93xzw8zbg0p1izqyzzc5mbiz\",\n  \"login\": \"twitchdev\",\n  \"scopes\": [\n    \"channel:read:subscriptions\"\n  ],\n  \"user_id\": \"141981764\",\n  \"expires_in\": 5520838\n}\n"
                )
        )


accessTokenFromUrlTest : Test
accessTokenFromUrlTest =
    describe "get access token from url"
        [ test "access token is present"
            (\_ ->
                let
                    urlString =
                        Url.fromString "https://localhost:3000/#access_token=73d0f8mkabpbmjp921asv2jaidwxn&scope=channel%3Amanage%3Apolls+channel%3Aread%3Apolls&state=c3ab8aa609ea11e793ae92361f002671&token_type=bearer"
                in
                case urlString of
                    Just u ->
                        Expect.equal (Just (Twitch.Token "73d0f8mkabpbmjp921asv2jaidwxn"))
                            (Twitch.accessTokenFromUrl u)

                    Nothing ->
                        Expect.fail "test string could not be parsed to url"
            )
        , test "access token is not present"
            (\_ ->
                let
                    urlString =
                        Url.fromString "https://localhost:3000/#scope=channel%3Amanage%3Apolls+channel%3Aread%3Apolls&state=c3ab8aa609ea11e793ae92361f002671&token_type=bearer"
                in
                case urlString of
                    Just u ->
                        Expect.equal
                            Nothing
                            (Twitch.accessTokenFromUrl u)

                    Nothing ->
                        Expect.fail "test string could not be parsed to url"
            )
        ]


decodeUserTest : Test
decodeUserTest =
    test "decode user"
        (\_ ->
            Expect.equal
                (Result.Ok
                    { id = "141981764", displayName = "TwitchDev", profileImageUrl = "https://static-cdn.jtvnw.net/jtv_user_pictures/8a6381c7-d0c0-4576-b179-38bd5ce1d6af-profile_image-300x300.png" }
                )
                (Decode.decodeString
                    Twitch.decodeUser
                    "{ \"id\": \"141981764\", \"login\": \"twitchdev\", \"display_name\": \"TwitchDev\", \"type\": \"\", \"broadcaster_type\": \"partner\", \"description\": \"Supporting third-party developers building Twitch integrations from chatbots to game integrations.\", \"profile_image_url\": \"https://static-cdn.jtvnw.net/jtv_user_pictures/8a6381c7-d0c0-4576-b179-38bd5ce1d6af-profile_image-300x300.png\", \"offline_image_url\": \"https://static-cdn.jtvnw.net/jtv_user_pictures/3f13ab61-ec78-4fe6-8481-8682cb3b0ac2-channel_offline_image-1920x1080.png\", \"view_count\": 5980557, \"email\": \"not-real@email.com\", \"created_at\": \"2016-12-14T20:32:28Z\"    }"
                )
        )


decodeFollowRelationTest : Test
decodeFollowRelationTest =
    test "decode follow relation"
        (\_ ->
            Expect.equal
                (Result.Ok
                    { fromID = "171003792", fromLogin = "iiisutha067iii", fromName = "IIIsutha067III", toID = "23161357", toName = "LIRIK", followedAt = "2017-08-22T22:55:24Z" }
                )
                (Decode.decodeString
                    Twitch.decodeFollowRelation
                    "{\"from_id\": \"171003792\",\"from_login\": \"iiisutha067iii\",\"from_name\": \"IIIsutha067III\",\"to_id\": \"23161357\",\"to_name\": \"LIRIK\",\"followed_at\": \"2017-08-22T22:55:24Z\"      }"
                )
        )


decodePaginatedTest : Test
decodePaginatedTest =
    describe "decode paginated responses"
        [ test
            "decode with cursor"
            (\_ ->
                Expect.equal
                    (Result.Ok
                        { cursor = Just "somecursor", data = "some value" }
                    )
                    (Decode.decodeString
                        (Twitch.decodePaginated
                            Decode.string
                        )
                        "{ \"pagination\": {\"cursor\": \"somecursor\"}, \"data\": \"some value\"}"
                    )
            )
        , test
            "decode without cursor"
            (\_ ->
                Expect.equal
                    (Result.Ok
                        { cursor = Nothing, data = "some value" }
                    )
                    (Decode.decodeString
                        (Twitch.decodePaginated
                            Decode.string
                        )
                        "{ \"pagination\": {}, \"data\": \"some value\"}"
                    )
            )
        ]


decodeListHeadTest : Test
decodeListHeadTest =
    describe "decode json list heads"
        [ test "decode list with len 2"
            (\_ ->
                Expect.equal
                    (Result.Ok 1)
                    (Decode.decodeString
                        (Twitch.decodeListHead
                            Decode.int
                        )
                        "[1,2]"
                    )
            )
        , test "decode list with len 1"
            (\_ ->
                Expect.equal
                    (Result.Ok 1)
                    (Decode.decodeString
                        (Twitch.decodeListHead
                            Decode.int
                        )
                        "[1]"
                    )
            )
        , test "decode empty list"
            (\_ ->
                Expect.err
                    (Decode.decodeString
                        (Twitch.decodeListHead
                            Decode.int
                        )
                        "[]"
                    )
            )
        ]


decodeCategoryTest : Test
decodeCategoryTest =
    test "decode category"
        (\_ ->
            Expect.equal
                (Ok { name = "Science & Technology" })
                (Decode.decodeString Twitch.decodeCategory "{ \"id\": \"509670\", \"name\": \"Science & Technology\"}")
        )


decodeSegmentTest : Test
decodeSegmentTest =
    test "decode segment"
        (\_ ->
            Expect.equal
                (Ok
                    { startTime = "2021-07-01T18:00:00Z"
                    , endTime = "2021-07-01T19:00:00Z"
                    , title = "TwitchDev Monthly Update // July 1, 2021"
                    , canceledUntil = Nothing
                    , category = Just { name = "Science & Technology" }
                    , isRecurring = False
                    }
                )
                (Decode.decodeString Twitch.decodeSegment "{\"start_time\":\"2021-07-01T18:00:00Z\", \"end_time\":\"2021-07-01T19:00:00Z\", \"title\":\"TwitchDev Monthly Update // July 1, 2021\", \"canceled_until\":null, \"category\":{\"id\":\"509670\", \"name\":\"Science & Technology\"}, \"is_recurring\":false}")
        )


decodeScheduleTest : Test
decodeScheduleTest =
    test "decode schedule"
        (\_ ->
            Expect.equal
                (Ok
                    { segments =
                        [ { startTime = "2021-07-01T18:00:00Z"
                          , endTime = "2021-07-01T19:00:00Z"
                          , title = "TwitchDev Monthly Update // July 1, 2021"
                          , canceledUntil = Nothing
                          , category = Just { name = "Science & Technology" }
                          , isRecurring = False
                          }
                        ]
                    , broadcasterId = "141981764"
                    , broadcasterName = "TwitchDev"
                    }
                )
                (Decode.decodeString Twitch.decodeSchedule "{\"segments\":[{\"start_time\":\"2021-07-01T18:00:00Z\",\"end_time\":\"2021-07-01T19:00:00Z\",\"title\":\"TwitchDev Monthly Update // July 1, 2021\",\"canceled_until\":null,\"category\":{\"id\":\"509670\",\"name\":\"Science & Technology\"},\"is_recurring\":false}],\"broadcaster_id\":\"141981764\",\"broadcaster_name\":\"TwitchDev\"}")
        )

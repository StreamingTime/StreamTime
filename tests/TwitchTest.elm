module TwitchTest exposing (accessTokenFromUrlTest, decodeValidateTokenResponseTest)

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
                        Expect.equal (Just "73d0f8mkabpbmjp921asv2jaidwxn")
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

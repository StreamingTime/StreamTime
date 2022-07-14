module TwitchTest exposing (..)

import Expect
import Json.Decode as Decode
import Test exposing (..)
import Twitch


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

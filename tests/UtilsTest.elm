module UtilsTest exposing (concatMaybeListTest, filterFollowsByLoginTest, missingProfileLoginsTest, missingStreamersInSchedulesTest, streamersInSchedulesTest, streamersWithSelectionTest)

import Expect
import Test exposing (Test, describe, test)
import Twitch
import Utils exposing (concatMaybeList, filterFollowsByLogin, missingProfileLogins, missingStreamersInSchedules, schedulesWithStreamers, streamersWithSelection)


concatMaybeListTest : Test
concatMaybeListTest =
    describe "test concatMaybeList"
        [ test "append to Nothing"
            (\_ ->
                Expect.equal [ 1, 2, 3 ] (concatMaybeList Nothing [ 1, 2, 3 ])
            )
        , test "append to Just"
            (\_ ->
                Expect.equal [ 1, 2, 3, 4, 5, 6 ] (concatMaybeList (Just [ 1, 2, 3 ]) [ 4, 5, 6 ])
            )
        ]


filterFollowsByLoginTest : Test
filterFollowsByLoginTest =
    let
        follows : List Twitch.FollowRelation
        follows =
            List.map
                (\login ->
                    { fromID = ""
                    , fromLogin = ""
                    , fromName = ""
                    , toID = ""
                    , toName = ""
                    , toLogin = login
                    , followedAt = ""
                    }
                )
                [ "foo", "bar", "baz" ]
    in
    describe "test filterFollowsByLogin"
        [ test "search existing item"
            (\_ ->
                follows
                    |> filterFollowsByLogin "foo"
                    |> List.map .toLogin
                    |> Expect.equal [ "foo" ]
            )
        , test "search existing item in different case"
            (\_ ->
                follows
                    |> filterFollowsByLogin "FOO"
                    |> List.map .toLogin
                    |> Expect.equal [ "foo" ]
            )
        , test "search non existing item"
            (\_ ->
                follows
                    |> filterFollowsByLogin "ABCDEF"
                    |> List.map .toLogin
                    |> Expect.equal []
            )
        ]


missingProfileLoginsTest : Test
missingProfileLoginsTest =
    test "test missingProfileLogins"
        (\_ ->
            let
                follows =
                    List.map
                        (\( id, login ) ->
                            { fromID = ""
                            , fromLogin = ""
                            , fromName = ""
                            , toID = id
                            , toName = ""
                            , toLogin = login
                            , followedAt = ""
                            }
                        )
                        [ ( "id1", "foo" ), ( "id2", "bar" ), ( "notFetchedId", "notFetched" ) ]

                streamers =
                    List.map
                        (\login ->
                            { id = ""
                            , displayName = ""
                            , profileImageUrl = ""
                            , loginName = login
                            }
                        )
                        [ "foo", "bar" ]
            in
            missingProfileLogins follows streamers
                |> Expect.equal [ "notFetchedId" ]
        )


streamersWithSelectionTest : Test
streamersWithSelectionTest =
    test "test streamers with selection"
        (\_ ->
            let
                selected =
                    List.map
                        (\login ->
                            { id = ""
                            , displayName = ""
                            , profileImageUrl = ""
                            , loginName = login
                            }
                        )
                        [ "foo", "bar" ]

                streamers =
                    List.map
                        (\login ->
                            { id = ""
                            , displayName = ""
                            , profileImageUrl = ""
                            , loginName = login
                            }
                        )
                        [ "foo", "bar", "notSelected" ]
            in
            streamersWithSelection selected streamers
                |> List.map (\( user, isSelected ) -> ( user.loginName, isSelected ))
                |> Expect.equal [ ( "foo", True ), ( "bar", True ), ( "notSelected", False ) ]
        )


missingStreamersInSchedulesTest : Test
missingStreamersInSchedulesTest =
    test "missing streamers in schedules"
        (\_ ->
            let
                missing =
                    [ { id = "3", displayName = "c", profileImageUrl = "", loginName = "c" } ]

                streamers =
                    [ { id = "1", displayName = "a", profileImageUrl = "", loginName = "a" }
                    , { id = "2", displayName = "b", profileImageUrl = "", loginName = "b" }
                    ]
                        ++ missing

                schedules =
                    [ { segments = [], broadcasterId = "1", broadcasterName = "a" }
                    , { segments = [], broadcasterId = "2", broadcasterName = "b" }
                    ]
            in
            missingStreamersInSchedules streamers schedules
                |> Expect.equal missing
        )


streamersInSchedulesTest : Test
streamersInSchedulesTest =
    test "streamers in schedules"
        (\_ ->
            let
                streamers =
                    [ { id = "1", displayName = "a", profileImageUrl = "", loginName = "a" }
                    ]

                schedules =
                    [ { segments = [], broadcasterId = "1", broadcasterName = "a" }
                    , { segments = [], broadcasterId = "2", broadcasterName = "b" }
                    ]
            in
            schedulesWithStreamers streamers schedules
                |> Expect.equal [ { segments = [], broadcasterId = "1", broadcasterName = "a" } ]
        )

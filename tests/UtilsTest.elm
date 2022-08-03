module UtilsTest exposing (concatMaybeListTest, filterFollowsByLoginTest, missingProfileLoginsTest, streamersWithSelectionTest, timeInOneWeekTest)

import Expect
import FormatTime
import Test exposing (Test, describe, test)
import Time
import Twitch
import Utils exposing (concatMaybeList, filterFollowsByLogin, missingProfileLogins, streamersWithSelection)


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


timeInOneWeekTest : Test
timeInOneWeekTest =
    test "calculate time in one week"
        (\_ ->
            Time.millisToPosix 2000000000000
                |> Utils.timeInOneWeek
                |> FormatTime.asRFC3339 Time.utc
                |> Expect.equal (Ok "2033-05-25T03:33:20+00:00")
        )

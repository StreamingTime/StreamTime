module RefreshDataTest exposing (isLoadingTest, mapTest, mapToTest, mapValueTest, unwrapTest)

import Expect
import Http
import RefreshData
import Test exposing (Test, describe, test)


mapTest : Test
mapTest =
    describe "test RefreshData.map"
        [ test "map to different state"
            (\_ ->
                RefreshData.Present "foo"
                    |> RefreshData.map RefreshData.LoadingMore
                    |> Expect.equal (RefreshData.LoadingMore "foo")
            )
        , test "map to different value"
            (\_ ->
                RefreshData.Present "foo"
                    |> RefreshData.map (\s -> RefreshData.Present (String.toUpper s))
                    |> Expect.equal (RefreshData.Present "FOO")
            )
        ]


mapToTest : Test
mapToTest =
    describe "test RefreshData.mapTo"
        [ test "map without error"
            (\_ ->
                RefreshData.Present "foo"
                    |> RefreshData.mapTo (\e _ -> e)
                    |> Expect.equal Nothing
            )
        , test "map with error"
            (\_ ->
                RefreshData.ErrorWithData Http.Timeout "foo"
                    |> RefreshData.mapTo (\e _ -> e)
                    |> Expect.equal (Just Http.Timeout)
            )
        , test "map to string"
            (\_ ->
                RefreshData.ErrorWithData Http.Timeout "foo"
                    |> RefreshData.mapTo (\_ _ -> "FOO")
                    |> Expect.equal "FOO"
            )
        ]


mapValueTest : Test
mapValueTest =
    test "map without change the state"
        (\_ ->
            RefreshData.Present "foo"
                |> RefreshData.mapValue String.toUpper
                |> Expect.equal (RefreshData.Present "FOO")
        )


unwrapTest : Test
unwrapTest =
    test "unwrap RefreshData"
        (\_ ->
            RefreshData.Present "foo"
                |> RefreshData.unwrap
                |> Expect.equal "foo"
        )


isLoadingTest : Test
isLoadingTest =
    describe "test RefreshData.isLoading"
        [ test "loading state"
            (\_ ->
                Expect.equal True (RefreshData.isLoading (RefreshData.LoadingMore "foo"))
            )
        , test
            "not loading state"
            (\_ ->
                let
                    values =
                        [ RefreshData.Present "", RefreshData.ErrorWithData Http.Timeout "" ]
                in
                Expect.equal [ False, False ] (List.map RefreshData.isLoading values)
            )
        ]

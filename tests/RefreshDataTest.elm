module RefreshDataTest exposing (refreshDataMapComparison)

import Expect
import Http
import RefreshData exposing (RefreshData(..))
import Test exposing (Test, describe, test)


refreshDataMapComparison : Test
refreshDataMapComparison =
    describe "compare two map implementations"
        [ test "simple map"
            (\_ ->
                let
                    data =
                        RefreshData.Present "foo"
                in
                Expect.equal
                    (RefreshData.LoadingMore "foo")
                    (RefreshData.map RefreshData.LoadingMore data)
            )

        {- , test "map to Error"
               (\_ ->
                   let
                       data =
                           RefreshData.Present "foo"
                   in
                   Expect.equal
                       (RefreshData.ErrorWithData Http.Timeout "foo")
                       (RefreshData.map identity (RefreshData.ErrorWithData Http.Timeout) data)
               )
           , test "map from Error should work"
               (\_ ->
                   let
                       data =
                           RefreshData.ErrorWithData Http.Timeout "oldValue"
                   in
                   Expect.equal
                       (RefreshData.Present "foo")
                       (RefreshData.map (\_ -> "foo") Present data)
               )
        -}
        ]

module UtilsTest exposing (concatMaybeListTest)

import Expect
import Test exposing (Test, describe, test)
import Utils exposing (concatMaybeList)


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

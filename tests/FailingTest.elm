module FailingTest exposing (..)

import Expect
import Test exposing (Test, test)


fail : Test
fail =
    test "this test fails" (\_ -> Expect.fail "test failed")

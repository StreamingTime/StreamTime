module ListTest exposing (filterIndexedMapTest)

import Expect
import List.Extra
import Test exposing (Test, test)


filterIndexedMapTest : Test
filterIndexedMapTest =
    test "filter indexed map"
        (\_ ->
            List.Extra.filterIndexedMap
                (\index number ->
                    String.toInt number
                        |> Maybe.map (\v -> ( index, v ))
                )
                [ "a", "0", "1", "2", "b", "3" ]
                |> Expect.equal [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ]
        )

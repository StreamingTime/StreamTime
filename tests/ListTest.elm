module ListTest exposing (filterIndexedMapTest, groupByTest)

import Dict
import Expect
import List.Extra
import Test exposing (Test, test)


groupByTest : Test
groupByTest =
    test "test grouping"
        (\_ ->
            [ 1, 2, 3, 4 ]
                |> List.Extra.groupBy
                    (\number ->
                        if modBy 2 number == 0 then
                            "Even"

                        else
                            "Odd"
                    )
                |> Expect.equal
                    (Dict.fromList
                        [ ( "Even", [ 2, 4 ] )
                        , ( "Odd", [ 1, 3 ] )
                        ]
                    )
        )


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

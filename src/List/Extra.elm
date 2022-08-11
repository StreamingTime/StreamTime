module List.Extra exposing (filterIndexedMap)


filterIndexedMap : (Int -> a -> Maybe b) -> List a -> List b
filterIndexedMap func =
    let
        filterIndexedMapHelper : Int -> List a -> List b
        filterIndexedMapHelper index l =
            case l of
                [] ->
                    []

                x :: xs ->
                    case func index x of
                        Just value ->
                            value :: filterIndexedMapHelper (index + 1) xs

                        Nothing ->
                            filterIndexedMapHelper index xs
    in
    filterIndexedMapHelper 0

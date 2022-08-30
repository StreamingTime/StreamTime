module List.Extra exposing (filterIndexedMap, groupBy)

import Dict exposing (Dict)


{-| Sort items of a List into a dict that associates multiple values with a key derived from the items

See tests/ListTest.elm for examples.

-}
groupBy : (v -> comparable) -> List v -> Dict comparable (List v)
groupBy toKey =
    let
        appendToGroup : Dict comparable (List v) -> comparable -> v -> Dict comparable (List v)
        appendToGroup dict groupKey value =
            Dict.update groupKey (\oldList -> Just (Maybe.withDefault [] oldList ++ [ value ])) dict

        helper : Dict comparable (List v) -> List v -> Dict comparable (List v)
        helper acc list =
            case list of
                [] ->
                    acc

                item :: rest ->
                    helper (appendToGroup acc (toKey item) item) rest
    in
    helper Dict.empty


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

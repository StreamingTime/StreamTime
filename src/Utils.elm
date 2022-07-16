module Utils exposing (concatMaybeList)

{- append a list to a Maybe list -}


concatMaybeList : Maybe (List a) -> List a -> List a
concatMaybeList maybeList list =
    case maybeList of
        Just l ->
            l ++ list

        Nothing ->
            list

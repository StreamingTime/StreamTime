module RefreshData exposing (RefreshData(..), isLoading, map, mapTo, mapValue, unwrap)

{-| `RefreshData` holds a value that may be replaced or updated later.
When the update is in progress or there is an error, it can indicate loading or stores the error without losing the data.
-}


type RefreshData errorType a
    = LoadingMore a
    | Present a
    | ErrorWithData errorType a


{-| Transform a `RefreshData` into any other type using as function.
Useful to unwrap the stored value

    mapTo (\_ -> identity) (Present "x") == "x"

    mapTo (\err _ -> err) (ErrorWithData Http.Timeout "x") == Just Timeout

-}
mapTo : (Maybe errorType -> a -> b) -> RefreshData errorType a -> b
mapTo func data =
    case data of
        LoadingMore value ->
            func Nothing value

        Present value ->
            func Nothing value

        ErrorWithData error value ->
            func (Just error) value


{-| Transform a `RefreshData` with a given function.
Useful to convert between two states.

    map LoadingMore (Present "x") == LoadingMore "x"

    map (ErrorWithData Http.Timeout) (LoadingMore "x") == ErrorWithData Timeout "x"

    map (\s -> Present (String.toUpper s)) (Present "x") == Present "X"

-}
map : (a -> RefreshData errorTypeB b) -> RefreshData errorType a -> RefreshData errorTypeB b
map func data =
    case data of
        LoadingMore value ->
            func value

        Present value ->
            func value

        ErrorWithData _ value ->
            func value


{-| Transform the stored value, without changing the state
-}
mapValue : (a -> b) -> RefreshData errorType a -> RefreshData errorType b
mapValue func data =
    case data of
        LoadingMore value ->
            LoadingMore (func value)

        Present value ->
            Present (func value)

        ErrorWithData err value ->
            ErrorWithData err (func value)

{-| Just give me the value please-}
unwrap : RefreshData errorType a -> a
unwrap =
    mapTo (\_ v -> v)


{-| Whether this `RefreshData` is currently waiting for (new) data
-}
isLoading : RefreshData errorType a -> Bool
isLoading data =
    case data of
        LoadingMore _ ->
            True

        Present _ ->
            False

        ErrorWithData _ _ ->
            False

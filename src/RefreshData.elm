module RefreshData exposing (RefreshData(..), isLoading, map, mapTo)

import Http


{-| `RefreshData` holds a value that may be replaced or updated later.
When the update is in progress or there is an error, it can indicate loading or stores the error without losing the data.
-}
type RefreshData a
    = LoadingMore a
    | Present a
    | ErrorWithData Http.Error a


{-| Transform a `RefreshData` into any other type using as function.
Useful to unwrap the stored value

    mapTo (\_ -> identity) (Present "x") == "x"

    mapTo (\err _ -> err) (ErrorWithData Http.Timeout "x") == Just Timeout

-}
mapTo : (Maybe Http.Error -> a -> b) -> RefreshData a -> b
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
map : (a -> RefreshData b) -> RefreshData a -> RefreshData b
map func data =
    case data of
        LoadingMore value ->
            func value

        Present value ->
            func value

        ErrorWithData _ value ->
            func value


{-| Whether this `RefreshData` is currently waiting for (new) data
-}
isLoading : RefreshData a -> Bool
isLoading data =
    case data of
        LoadingMore _ ->
            True

        Present _ ->
            False

        ErrorWithData _ _ ->
            False

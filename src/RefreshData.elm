module RefreshData exposing (RefreshData(..), isLoading, map, mapError, toMaybe, withDefault, withError)

import Http



{- RefreshData holds a value that may be replaced or updated later.
   When the update is in progress or there is an error, it can indicate loading or store the error without losing the data.
-}


type RefreshData a
    = --add Uninitialized variant neccessary
      Initialising
    | LoadingMore a
    | Present a
    | Error Http.Error
    | ErrorWithData Http.Error a


withDefault : a -> RefreshData a -> a
withDefault default inc =
    case inc of
        LoadingMore values ->
            values

        Present values ->
            values

        ErrorWithData _ values ->
            values

        Error _ ->
            default

        Initialising ->
            default


toMaybe : RefreshData a -> Maybe a
toMaybe inc =
    case inc of
        LoadingMore value ->
            Just value

        Present value ->
            Just value

        ErrorWithData _ value ->
            Just value

        Error _ ->
            Nothing

        Initialising ->
            Nothing



{- Transform the value, if present -}


map : (Maybe a -> RefreshData b) -> RefreshData a -> RefreshData b
map func inc =
    func (toMaybe inc)



{- Transform the error value, if present -}


mapError : (Maybe Http.Error -> b) -> RefreshData a -> b
mapError func data =
    case data of
        LoadingMore _ ->
            func Nothing

        Present _ ->
            func Nothing

        ErrorWithData error _ ->
            func (Just error)

        Error error ->
            func (Just error)

        Initialising ->
            func Nothing



{- Transform the current state into a state that contains the given error and the eventually present data -}


withError : Http.Error -> RefreshData a -> RefreshData a
withError err data =
    case toMaybe data of
        Just value ->
            ErrorWithData err value

        Nothing ->
            Error err



{- whether this RefreshData is currently waiting for (new) data -}


isLoading : RefreshData a -> Bool
isLoading data =
    case data of
        LoadingMore _ ->
            True

        Initialising ->
            True

        Present _ ->
            False

        ErrorWithData _ _ ->
            False

        Error _ ->
            False

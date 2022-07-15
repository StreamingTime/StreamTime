module Data exposing (Data(..), fromResult)

import Http


type Data value
    = Loading
    | Failure Http.Error
    | Success value


fromResult : Result Http.Error a -> Data a
fromResult result =
    case result of
        Err e ->
            Failure e

        Ok x ->
            Success x

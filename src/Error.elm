module Error exposing (Error(..), toString)

import Http


type Error
    = HttpError Http.Error
    | StringError String


toString : Error -> String
toString error =
    case error of
        StringError s ->
            s

        HttpError err ->
            httpErrorToString err


httpErrorToString : Http.Error -> String
httpErrorToString error =
    let
        networkProblem =
            "Failed to connect to the server. Is your internet ok?"

        generalProblem =
            "There was a problem :("
    in
    case error of
        Http.Timeout ->
            networkProblem

        Http.NetworkError ->
            networkProblem

        Http.BadUrl _ ->
            generalProblem

        Http.BadBody _ ->
            generalProblem

        Http.BadStatus _ ->
            generalProblem

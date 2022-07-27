module Utils exposing (concatMaybeList, errorToString, filterFollowsByLogin, missingProfileLogins, streamersWithSelection)

import Http
import Twitch



{- append a list to a Maybe list -}


concatMaybeList : Maybe (List a) -> List a -> List a
concatMaybeList maybeList list =
    case maybeList of
        Just l ->
            l ++ list

        Nothing ->
            list


filterFollowsByLogin : String -> List Twitch.FollowRelation -> List Twitch.FollowRelation
filterFollowsByLogin name =
    List.filter
        (\f ->
            f.toLogin
                |> String.toLower
                |> String.contains
                    (String.toLower name)
        )



{- Compile a list of all user IDs for streamers that are part of the follows list, but whos profiles are not in the streamers list
   (aka: the details are not fetched yet)
-}


missingProfileLogins : List Twitch.FollowRelation -> List Twitch.User -> List String
missingProfileLogins follows streamers =
    follows
        |> List.filterMap
            (\follow ->
                if List.any (\streamer -> follow.toLogin == streamer.loginName) streamers then
                    Nothing

                else
                    Just follow.toID
            )



{- pair every item of the second list with a bool indicating whether it is part of the selected streamers list -}


streamersWithSelection : List Twitch.User -> List Twitch.User -> List ( Twitch.User, Bool )
streamersWithSelection selected users =
    users
        |> List.map (\u -> ( u, List.any ((==) u) selected ))


errorToString : Http.Error -> String
errorToString error =
    let
        networkProblem =
            "Failed to connect to the server."

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

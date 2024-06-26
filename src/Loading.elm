module Loading exposing (loadInitialData)

{-| The Loading module contains code we need at the start of our application, like verifying the token or fetching which streamers our user follows.
For convenience, it also defines datatypes we need in our main application model, SignedInUser and AppData.

Loading happens in multiple stages. The use of multiple, growing data types allows us to avoid the use of Maybe, which reduces complexity and avoids the need
to handle states that should not be able to occur.


# Steps:

  - validate the token
  - get our users profile
  - get the browser time and zone
  - get user ids of streamers our user follows
  - get some of their profiles

-}

import Http
import RefreshData
import Task
import Time
import Twitch
import TwitchConfig
import Types exposing (AppData, SignedInUser)


type alias WithVerifiedToken =
    { token : Twitch.Token
    , loginName : String
    , userID : Twitch.UserID
    }


type alias WithFullUser =
    SignedInUser


type alias WithTimeInfo =
    { signedInUser : SignedInUser
    , zone : Time.Zone
    , time : Time.Posix
    }


type alias WithFollows =
    { signedInUser : SignedInUser
    , zone : Time.Zone
    , time : Time.Posix
    , follows : List Twitch.FollowRelation
    }


{-| All the data we need for our application
-}
validateToken : Twitch.Token -> Task.Task Http.Error WithVerifiedToken
validateToken token =
    Twitch.validateTokenTask token
        |> Task.map
            (\{ login, userID } ->
                { token = token
                , loginName = login
                , userID = userID
                }
            )


getUserInfo : WithVerifiedToken -> Task.Task Http.Error WithFullUser
getUserInfo { token } =
    Twitch.getLoggedInUserTask TwitchConfig.clientId token
        |> Task.map
            (\{ id, displayName, profileImageUrl, loginName } ->
                { token = token
                , userID = id
                , loginName = loginName
                , displayName = displayName
                , profileImageUrl = profileImageUrl
                }
            )


getTimeStuff : WithFullUser -> Task.Task Http.Error WithTimeInfo
getTimeStuff signedInUser =
    Task.map2
        (\time zone ->
            { signedInUser = signedInUser, time = time, zone = zone }
        )
        Time.now
        Time.here


getFollows : WithTimeInfo -> Task.Task Http.Error WithFollows
getFollows { signedInUser, time, zone } =
    let
        fetch cursor =
            Twitch.getUserFollowsTask signedInUser.userID cursor TwitchConfig.clientId signedInUser.token
    in
    fetch
        |> Twitch.allPages
        |> Task.map
            (\follows ->
                { signedInUser = signedInUser
                , time = time
                , zone = zone
                , follows = follows
                }
            )


getFirstStreamerProfiles : Int -> WithFollows -> Task.Task Http.Error AppData
getFirstStreamerProfiles streamerListPageSteps { follows, signedInUser, time, zone } =
    let
        streamerIDs =
            follows
                |> List.take streamerListPageSteps
                |> List.map .broadcasterId
    in
    Twitch.getUsersTask streamerIDs TwitchConfig.clientId signedInUser.token
        |> Task.map
            (\streamers ->
                { signedInUser = signedInUser
                , streamers = RefreshData.Present streamers
                , follows = follows
                , timeZone = zone
                , time = time
                , sidebarStreamerCount = streamerListPageSteps
                , streamerFilterName = Nothing
                , schedules = RefreshData.LoadingMore []
                , selectedStreamers = []
                , tab = Types.ScheduleTab
                , videos = RefreshData.LoadingMore []
                }
            )


loadInitialData : Twitch.Token -> Int -> Cmd (Result Http.Error AppData)
loadInitialData token sidebarStreamerCount =
    validateToken token
        |> Task.andThen getUserInfo
        |> Task.andThen getTimeStuff
        |> Task.andThen getFollows
        |> Task.andThen (getFirstStreamerProfiles sidebarStreamerCount)
        |> Task.attempt identity

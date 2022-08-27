module Types exposing (AppData, SignedInUser, Tab(..))

import Error exposing (Error)
import RefreshData exposing (RefreshData)
import Time
import Twitch


type Tab
    = ScheduleTab
    | VideoTab


type alias AppData =
    { signedInUser : SignedInUser
    , streamers : RefreshData Error (List Twitch.User)

    -- a list of follow relation metadata originating from our user
    , follows : List Twitch.FollowRelation
    , sidebarStreamerCount : Int
    , streamerFilterName : Maybe String
    , selectedStreamers : List Twitch.User
    , schedules : RefreshData Error (List Twitch.Schedule)
    , timeZone : Time.Zone
    , videos : RefreshData Error (List Twitch.Video)
    , time : Time.Posix
    , tab : Tab
    }


{-| All information directly related to our logged in user
-}
type alias SignedInUser =
    { token : Twitch.Token
    , loginName : String
    , userID : Twitch.UserID
    , displayName : String
    , profileImageUrl : String
    }

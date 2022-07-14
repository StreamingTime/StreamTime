module Twitch exposing (..)

import Url.Builder



{-
   Construct a URL for the Twitch "Implicit grant flow" authentication method
-}


loginFlowUrl : String -> String -> String
loginFlowUrl clientID redirectUri =
    Url.Builder.crossOrigin "https://id.twitch.tv"
        [ "oauth2", "authorize" ]
        [ Url.Builder.string "client_id" clientID
        , Url.Builder.string "redirect_uri" redirectUri
        , Url.Builder.string "response_type" "token"
        ]

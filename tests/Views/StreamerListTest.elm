module Views.StreamerListTest exposing (streamerDataStatusTest)

import Html.Styled exposing (toUnstyled)
import Http
import RefreshData
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)
import Views.StreamerList exposing (streamerListView)


streamerDataStatusTest : Test
streamerDataStatusTest =
    describe "streaming data status"
        [ test "with error"
            (\_ ->
                let
                    streamers =
                        RefreshData.ErrorWithData Http.Timeout []
                in
                streamerListView streamers [] 10 Nothing
                    |> toUnstyled
                    |> Query.fromHtml
                    |> Query.has [ text "Failed to connect to the server." ]
            )
        , test "present"
            (\_ ->
                let
                    streamers =
                        RefreshData.Present []
                in
                streamerListView streamers [] 10 Nothing
                    |> toUnstyled
                    |> Query.fromHtml
                    |> Query.hasNot [ text "Failed to connect to the server." ]
            )
        , test "loading"
            (\_ ->
                let
                    streamers =
                        RefreshData.LoadingMore []
                in
                streamerListView streamers [] 10 Nothing
                    |> toUnstyled
                    |> Query.fromHtml
                    |> Query.hasNot [ text "Failed to connect to the server." ]
            )
        ]

module Views.Components exposing (errorView, loadingSpinner)

import Css
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css, style)
import Icons
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


loadingSpinner : List Css.Style -> Html msg
loadingSpinner styles =
    div
        [ css
            (List.concat
                [ styles
                , [ Tw.border_4
                  , Tw.border_solid
                  , Tw.border_color Theme.white
                  , Tw.rounded_full
                  , Tw.animate_spin
                  ]
                ]
            )
        , style "border-top-color" "transparent"
        ]
        []


errorView : String -> Html msg
errorView errMsg =
    div
        [ css
            [ Tw.alert
            , Tw.alert_error
            , Tw.flex
            , Tw.justify_center
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.items_center
                , Tw.space_x_4
                ]
            ]
            [ div [] [ Icons.warning [ Tw.w_8, Tw.icon_error ] ]
            , div [] [ text errMsg ]
            ]
        ]

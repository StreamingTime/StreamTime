module Components exposing (loadingSpinner)

import Css
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css, style)
import Tailwind.Utilities as Tw


loadingSpinner : List Css.Style -> Html msg
loadingSpinner styles =
    div
        [ css
            (List.concat
                [ styles
                , [ Tw.border_4
                  , Tw.border_solid
                  , Tw.border_white
                  , Tw.rounded_full
                  , Tw.animate_spin
                  ]
                ]
            )
        , style "border-top-color" "transparent"
        ]
        []

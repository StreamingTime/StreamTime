module Icons exposing (close, repeat, warning)

import Css
import Html.Styled exposing (Html)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes as SvgAttr exposing (d)


{-| <https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsoutlined/repeat/default/48px.svg>
-}
repeat : List Css.Style -> Html msg
repeat styles =
    svg
        [ SvgAttr.viewBox "0 0 48 48"
        , SvgAttr.css styles
        ]
        [ path
            [ d "m14 44-8-8 8-8 2.1 2.2-4.3 4.3H35v-8h3v11H11.8l4.3 4.3Zm-4-22.5v-11h26.2l-4.3-4.3L34 4l8 8-8 8-2.1-2.2 4.3-4.3H13v8Z"
            ]
            []
        ]


{-| <https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsrounded/warning/default/48px.svg>
-}
warning : List Css.Style -> Html msg
warning styles =
    svg
        [ SvgAttr.viewBox "0 0 48 48"
        , SvgAttr.css styles
        ]
        [ path
            [ d "M24.05 24.45ZM4.6 42q-.85 0-1.3-.75-.45-.75 0-1.5l19.4-33.5q.45-.75 1.3-.75.85 0 1.3.75l19.4 33.5q.45.75 0 1.5t-1.3.75Zm19.6-22.6q-.65 0-1.075.425-.425.425-.425 1.075v8.2q0 .65.425 1.075.425.425 1.075.425.65 0 1.075-.425.425-.425.425-1.075v-8.2q0-.65-.425-1.075-.425-.425-1.075-.425Zm0 16.75q.65 0 1.075-.425.425-.425.425-1.075 0-.65-.425-1.075-.425-.425-1.075-.425-.65 0-1.075.425Q22.7 34 22.7 34.65q0 .65.425 1.075.425.425 1.075.425ZM7.2 39h33.6L24 10Z"
            ]
            []
        ]


{-| <https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsoutlined/close/default/48px.svg>
-}
close : List Css.Style -> Html msg
close styles =
    svg
        [ SvgAttr.viewBox "0 0 48 48"
        , SvgAttr.css styles
        ]
        [ path
            [ d "m12.45 37.65-2.1-2.1L21.9 24 10.35 12.45l2.1-2.1L24 21.9l11.55-11.55 2.1 2.1L26.1 24l11.55 11.55-2.1 2.1L24 26.1Z"
            ]
            []
        ]

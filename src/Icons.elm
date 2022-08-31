module Icons exposing (checkCircle, close, info, repeat, warning)

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


{-| <https://fonts.google.com/icons?selected=Material%20Symbols%20Outlined%3Acheck_circle%3AFILL%400%3Bwght%40400%3BGRAD%400%3Bopsz%4048>
-}
checkCircle : List Css.Style -> Html msg
checkCircle styles =
    svg
        [ SvgAttr.viewBox "0 0 48 48"
        , SvgAttr.css styles
        ]
        [ path [ d "M21.05 33.1 35.2 18.95l-2.3-2.25-11.85 11.85-6-6-2.25 2.25ZM24 44q-4.1 0-7.75-1.575-3.65-1.575-6.375-4.3-2.725-2.725-4.3-6.375Q4 28.1 4 24q0-4.15 1.575-7.8 1.575-3.65 4.3-6.35 2.725-2.7 6.375-4.275Q19.9 4 24 4q4.15 0 7.8 1.575 3.65 1.575 6.35 4.275 2.7 2.7 4.275 6.35Q44 19.85 44 24q0 4.1-1.575 7.75-1.575 3.65-4.275 6.375t-6.35 4.3Q28.15 44 24 44Zm0-3q7.1 0 12.05-4.975Q41 31.05 41 24q0-7.1-4.95-12.05Q31.1 7 24 7q-7.05 0-12.025 4.95Q7 16.9 7 24q0 7.05 4.975 12.025Q16.95 41 24 41Zm0-17Z" ]
            []
        ]


{-| <https://fonts.google.com/icons?selected=Material%20Symbols%20Outlined%3Ainfo%3AFILL%400%3Bwght%40400%3BGRAD%400%3Bopsz%4048>
-}
info : List Css.Style -> Html msg
info styles =
    svg
        [ SvgAttr.viewBox "0 0 48 48"
        , SvgAttr.css styles
        ]
        [ path [ d "M22.65 34h3V22h-3ZM24 18.3q.7 0 1.175-.45.475-.45.475-1.15t-.475-1.2Q24.7 15 24 15q-.7 0-1.175.5-.475.5-.475 1.2t.475 1.15q.475.45 1.175.45ZM24 44q-4.1 0-7.75-1.575-3.65-1.575-6.375-4.3-2.725-2.725-4.3-6.375Q4 28.1 4 23.95q0-4.1 1.575-7.75 1.575-3.65 4.3-6.35 2.725-2.7 6.375-4.275Q19.9 4 24.05 4q4.1 0 7.75 1.575 3.65 1.575 6.35 4.275 2.7 2.7 4.275 6.35Q44 19.85 44 24q0 4.1-1.575 7.75-1.575 3.65-4.275 6.375t-6.35 4.3Q28.15 44 24 44Zm.05-3q7.05 0 12-4.975T41 23.95q0-7.05-4.95-12T24 7q-7.05 0-12.025 4.95Q7 16.9 7 24q0 7.05 4.975 12.025Q16.95 41 24.05 41ZM24 24Z" ]
            []
        ]

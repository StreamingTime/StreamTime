module Icons exposing (repeat)

import Html.Styled exposing (Html)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes as SvgAttr exposing (d, fill, stroke)


{-| <https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsoutlined/repeat/default/48px.svg>
-}
repeat : Int -> Int -> Html msg
repeat width height =
    svg
        [ SvgAttr.viewBox "0 0 48 48"
        , SvgAttr.height (String.fromInt width)
        , SvgAttr.width (String.fromInt height)
        ]
        [ path
            [ d "m14 44-8-8 8-8 2.1 2.2-4.3 4.3H35v-8h3v11H11.8l4.3 4.3Zm-4-22.5v-11h26.2l-4.3-4.3L34 4l8 8-8 8-2.1-2.2 4.3-4.3H13v8Z"
            , stroke "white"
            , fill "white"
            ]
            []
        ]

module Graphics exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Attributes as SA

iconAdd : Svg msg
iconAdd =
    svg
        [width "14", height "14"]
        [ line [class "iconsLine", x1 "1", y1 "7", x2 "13", y2 "7"] []
        , line [class "iconsLine", x1 "7", y1 "1", x2 "7", y2 "13"] []
        ]

iconClose : Svg msg
iconClose =
    svg
        [width "14", height "14"]
        [ line [class "iconsLine", x1 "1", y1 "1", x2 "13", y2 "13"] []
        , line [class "iconsLine", x1 "1", y1 "13", x2 "13", y2 "1"] []
        ]

iconEdit : Svg msg
iconEdit =
    svg
        [width "14", height "14"]
        [ line [class "iconsLine", SA.style "stroke-width:1", x1 "1", y1 "13", x2 "13", y2 "13"] []
        , line [class "iconsLine", x1 "2", y1 "13", x2 "12", y "1"] []
        ]

iconExit : Svg msg
iconExit =
    svg
        [width "14", height "14"]
        [ polyline [class "iconsLine", points "9,1 2,1 2,12 9,12"] []
        , polyline [class "iconsBlock", points "6,5 6,7 10,7 10,8 13,6 10,4 10,5"] []
        ]

iconLogin : Svg msg
iconLogin =
    --
    -- TODO : l'icone de login
    --
    svg [width "30", height "30"] []

iconValid : Svg msg
iconValid =
    svg
        [width "14", height "14"]
        [polyline [class "iconsLine", points "1,5 5,13 13,1"] []]

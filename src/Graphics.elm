module Graphics exposing (..)

import Array exposing (Array)
import Html exposing (Html, div)
import Html.Attributes as HA
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

iconValid : Svg msg
iconValid =
    svg
        [width "14", height "14"]
        [polyline [class "iconsLine", points "1,5 5,13 13,1"] []]

type alias Indicators =
    { wait : Int
    , wip : Int
    , done : Int
    }

progressBar : Indicators -> Html msg
progressBar indicators =
    let
        total = indicators.wait + indicators.wip + indicators.done
        donePercent = (String.fromFloat (toFloat indicators.done / toFloat total * 100))++"%"
        wipPercent = (String.fromFloat (toFloat indicators.wip / toFloat total * 100))++"%"
        waitPercent = (String.fromFloat (toFloat indicators.wait / toFloat total * 100))++"%"
    in
    div [HA.class "progress_bar"]
        [ div [class "done_color", HA.style "width" donePercent] []
        , div [class "wip_color", HA.style "width" wipPercent] []
        , div [class "wait_color", HA.style "width" waitPercent] []
        ]

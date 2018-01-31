module Pieces.Ball exposing (..)

import Html exposing (Html)
import Svg exposing (Svg, rect)
import Svg.Attributes exposing (width, height, fill, x, y)


ball : ( Float, Float ) -> Html msg
ball ( xPosition, yPosition ) =
    rect
        [ width "1.5"
        , height "1.5"
        , fill "#C64947"
        , x (toString xPosition)
        , y (toString yPosition)
        ]
        []

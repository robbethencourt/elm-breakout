module Pieces.Ball exposing (..)

import Html exposing (Html)
import Svg exposing (Svg, rect)
import Svg.Attributes exposing (width, height, fill, x, y)


type BallDirection
    = Up
    | Down


reverseBallDirection : BallDirection -> BallDirection
reverseBallDirection bd =
    case bd of
        Up ->
            Down

        Down ->
            Up


ball : ( Float, Float ) -> Html msg
ball ( xPosition, yPosition ) =
    rect
        [ width "1"
        , height "1"
        , fill "#C64947"
        , x (toString xPosition)
        , y (toString yPosition)
        ]
        []

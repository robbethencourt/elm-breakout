module Pieces.Paddle exposing (Paddle(..), normalPaddle, shortPaddle, renderPaddle)

import Svg exposing (Svg, rect)
import Svg.Attributes exposing (width, height, fill, x, y)


type Paddle
    = NormalPaddle PaddleSize
    | ShortPaddle PaddleSize


type alias PaddleSize =
    { left : Left
    , leftMiddle : LeftMiddle
    , middle : Middle
    , rightMiddle : RightMiddle
    , right : Right
    }


type alias Left =
    Int


type alias LeftMiddle =
    Int


type alias Middle =
    Int


type alias RightMiddle =
    Int


type alias Right =
    Int


normalPaddle : Paddle
normalPaddle =
    NormalPaddle <| PaddleSize 0 4 8 12 16


shortPaddle : Paddle
shortPaddle =
    ShortPaddle <| PaddleSize 0 2 4 6 8



---- html ----


renderPaddle : Int -> Paddle -> Svg.Svg msg
renderPaddle xPosition paddle =
    rect
        [ case paddle of
            NormalPaddle paddleSections ->
                width (toString (paddleSections.right + 4))

            ShortPaddle paddleSections ->
                width (toString (paddleSections.right + 2))
        , height "1.5"
        , fill "#C64947"
        , x (toString xPosition)
        , y "88.5"
        ]
        []

module Pieces.Paddle exposing (Paddle, initialPaddle, renderPaddle)

import Svg exposing (Svg, rect)
import Svg.Attributes exposing (width, height, fill, x, y)


type alias Paddle =
    { left : Left
    , leftMiddle : LeftMiddle
    , middle : Middle
    , rightMiddle : RightMiddle
    , right : Right
    }



-- type PaddleSection
--     = Left
--     | LeftMiddle
--     | Middle
--     | RightMiddle
--     | Right


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


initialPaddle : Paddle
initialPaddle =
    Paddle 0 4 8 12 16



---- html ----


renderPaddle : Int -> Svg.Svg msg
renderPaddle xPosition =
    rect
        [ width (toString (initialPaddle.right + 4))
        , height "1.5"
        , fill "#C64947"
        , x (toString xPosition)
        , y "88.5"
        ]
        []

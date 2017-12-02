module Pieces.Block exposing (Block, initialBlocks, rowOfSvgBlocks, getRowOfBlocks)

import Svg exposing (Svg, rect)
import Svg.Attributes exposing (width, height, fill, x, y)


type alias Block =
    { fillColor : String
    , value : Int
    , yPosition : Int
    , xPosition : Int
    }


createBlockList : String -> Int -> Int -> List Block
createBlockList fillColor value yPosition =
    List.map (Block fillColor value yPosition) (List.map ((*) 5) (List.range 0 19))


initialBlocks : List Block
initialBlocks =
    createBlockList "#CB4744" 10 10
        ++ createBlockList "#C76C3A" 8 12
        ++ createBlockList "#B47830" 6 14
        ++ createBlockList "#9FA426" 4 16
        ++ createBlockList "#46A047" 2 18
        ++ createBlockList "#4546C9" 1 20



---- html ----


rowOfSvgBlocks : List Block -> List (Svg msg)
rowOfSvgBlocks blocks =
    List.map svgBlock blocks


svgBlock : Block -> Svg msg
svgBlock { fillColor, value, yPosition, xPosition } =
    rect
        [ width "5%"
        , height "2"
        , fill fillColor
        , y (toString yPosition)
        , x ((toString xPosition) ++ "%")
        ]
        []



---- helpers ----


getRowOfBlocks : Int -> List Block -> List Block
getRowOfBlocks ballY blocks =
    List.filter (\block -> block.yPosition == ballY) blocks
module Pieces.Block exposing (Block, initBlocks, rowOfSvgBlocks, getRowOfBlocks, getBlock)

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


initBlocks : List Block
initBlocks =
    createBlockList "#CB4744" 7 8
        ++ createBlockList "#C76C3A" 7 10
        ++ createBlockList "#B47830" 4 12
        ++ createBlockList "#9FA426" 4 14
        ++ createBlockList "#46A047" 1 16
        ++ createBlockList "#4546C9" 1 18



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


getBlock : Int -> List Block -> Maybe Block
getBlock ballX blocks =
    blocks
        |> List.filter (\block -> block.xPosition == ballX)
        |> List.head

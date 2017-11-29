module Config exposing (..)

import Color
import Array
import Char


blockSize =
    20


boardWidth =
    10


boardHeight =
    22


blockPadding =
    2


emptyBlockColor =
    Color.black


boardPixelWidth =
    blockSize * boardWidth + (blockPadding * (boardWidth + 1))


boardPixelHeight =
    blockSize * boardHeight + (blockPadding * (boardHeight + 1))


emptyLine =
    Array.repeat boardWidth emptyBlockColor

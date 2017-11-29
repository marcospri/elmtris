module TetrisBoard exposing (..)

import Array
import Color


type alias TetrisLine =
    Array.Array Color.Color


type alias TetrisBoard =
    Array.Array TetrisLine


isLineClear : Color.Color -> TetrisLine -> Bool
isLineClear emptyBlockColor line =
    line
        |> Array.toList
        |> List.all (\b -> b /= emptyBlockColor)


countClearLines : Color.Color -> TetrisBoard -> Int
countClearLines emptyBlockColor board =
    board
        |> Array.filter (isLineClear emptyBlockColor)
        |> Array.length


moveLineDown : Int -> Int -> TetrisLine -> TetrisBoard -> TetrisBoard
moveLineDown positions lineIndex line board =
    if positions == 0 then
        board
    else
        Array.set (lineIndex + positions) line board


moveBoardLinesDown : Color.Color -> TetrisBoard -> TetrisBoard
moveBoardLinesDown emptyBlockColor board =
    let
        ( clearedLines, _, newBoard ) =
            board
                |> Array.foldr
                    (\l ( downPositions, lineIndex, b ) ->
                        let
                            nextLineIndex =
                                lineIndex - 1

                            isClear =
                                isLineClear emptyBlockColor l

                            newDownPositions =
                                if isClear then
                                    downPositions + 1
                                else
                                    downPositions
                        in
                            ( newDownPositions, nextLineIndex, moveLineDown downPositions lineIndex l b )
                    )
                    ( 0, (board |> Array.length) - 1, board )
    in
        newBoard

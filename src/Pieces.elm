module Pieces exposing (Piece, allPieces, movePiece, rotatePiece)

import Color


type alias Piece =
    { shapes : List (List (List Int))
    , color : Color.Color
    , current_shape : Int
    , x : Int
    , y : Int
    }


iPiece =
    { shapes =
        [ [ [ 0, 0, 0, 0 ], [ 1, 1, 1, 1 ], [ 0, 0, 0, 0 ] ], [ [ 1 ], [ 1 ], [ 1 ], [ 1 ] ] ]
    , color = Color.yellow
    , current_shape = 1
    , x = 3
    , y = 0
    }


oPiece =
    { shapes =
        [ [ [ 1, 1 ], [ 1, 1 ] ] ]
    , color = Color.blue
    , current_shape = 0
    , x = 3
    , y = 0
    }


tPiece =
    { shapes =
        [ [ [ 0, 1, 0 ], [ 1, 1, 1 ] ]
        , [ [ 1, 0 ], [ 1, 1 ], [ 1, 0 ] ]
        , [ [ 1, 1, 1 ], [ 0, 1, 0 ] ]
        , [ [ 0, 1 ], [ 1, 1 ], [ 0, 1 ] ]
        ]
    , color = Color.orange
    , current_shape = 0
    , x = 3
    , y = 0
    }


zPiece =
    { shapes =
        [ [ [ 1, 1, 0 ], [ 0, 1, 1 ] ]
        , [ [ 0, 1 ], [ 1, 1 ], [ 1, 0 ] ]
        ]
    , color = Color.red
    , current_shape = 0
    , x = 3
    , y = 0
    }


sPiece =
    { shapes =
        [ [ [ 0, 1, 1 ], [ 1, 1, 0 ] ]
        , [ [ 1, 0 ], [ 1, 1 ], [ 0, 1 ] ]
        ]
    , color = Color.green
    , current_shape = 0
    , x = 3
    , y = 0
    }


jPiece =
    { shapes =
        [ [ [ 0, 1 ], [ 0, 1 ], [ 1, 1 ] ]
        , [ [ 1, 0, 0 ], [ 1, 1, 1 ] ]
        , [ [ 1, 1 ], [ 1, 0 ], [ 1, 0 ] ]
        , [ [ 1, 1, 1 ], [ 0, 0, 1 ] ]
        ]
    , color = Color.purple
    , current_shape = 0
    , x = 3
    , y = 0
    }


lPiece =
    { shapes =
        [ [ [ 1, 0 ], [ 1, 0 ], [ 1, 1 ] ]
        , [ [ 1, 1, 1 ], [ 1, 0, 0 ] ]
        , [ [ 1, 1 ], [ 0, 1 ], [ 0, 1 ] ]
        , [ [ 0, 0, 1 ], [ 1, 1, 1 ] ]
        ]
    , color = Color.white
    , current_shape = 0
    , x = 3
    , y = 0
    }


movePieceY dy piece boardHeight =
    if dy == 0 then
        piece
    else
        { piece | y = Basics.min (boardHeight - 1) (Basics.max 0 (piece.y + dy)) }


movePieceX dx piece boardWidth =
    if dx == 0 then
        piece
    else
        { piece | x = Basics.min (boardWidth - 1) (Basics.max 0 (piece.x + dx)) }


movePiece dx dy piece ( boardWidth, boardHeight ) =
    let
        xPiece =
            movePieceX dx piece boardWidth
    in
        movePieceY dy xPiece boardHeight


rotatePiece piece =
    let
        nShapes =
            List.length piece.shapes
    in
        { piece | current_shape = (piece.current_shape + 1) % nShapes }


allPieces =
    [ iPiece, oPiece, tPiece, sPiece, zPiece, jPiece, lPiece ]

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Random.List exposing (choose, shuffle)
import Array
import Svg
import Task
import Random
import Time
import List
import Keyboard
import Color
import Debug
import List
import Svg exposing (rect, svg, text_)
import Svg.Attributes exposing (x, y, width, height, rx, ry, fill, textAnchor)
import TetrisBoard exposing (TetrisBoard, moveBoardLinesDown, countClearLines)
import Pieces exposing (Piece, allPieces, movePiece, rotatePiece)
import Config exposing (..)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



type alias Model =
    { board : TetrisBoard
    , piece : Maybe Piece
    , piecesBag : List Piece
    , randomSeed : Random.Seed
    , currentLevel : Int
    , levelLines : Int
    , points : Int
    , gameOver : Bool
    , ticks : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { board = (Array.repeat boardHeight emptyLine)
      , piece = Nothing
      , piecesBag = []
      , randomSeed = Random.initialSeed 0
      , currentLevel = 1
      , levelLines = 0
      , points = 0
      , gameOver = False
      , ticks = 0
      }
    , Task.perform SeedTime Time.now
    )



-- UPDATE


type Msg
    = KeyDown Int
    | Tick Time.Time
    | SeedTime Time.Time
    | Drop


blockInBoard ( x, y ) =
    if x >= boardWidth || x < 0 || y >= boardHeight || y < 0 then
        False
    else
        True


blockOccupied board ( x, y ) =
    if getBoardBlock ( x, y ) board == emptyBlockColor then
        True
    else
        False


pieceInBoard : Piece -> Bool
pieceInBoard piece =
    (getPieceCoordinates piece)
        |> List.all blockInBoard


pieceInEmptyBoard piece board =
    (getPieceCoordinates piece)
        |> List.all (blockOccupied board)


isAllowedMove : Piece -> TetrisBoard -> Bool
isAllowedMove piece board =
    (pieceInBoard piece) && (pieceInEmptyBoard piece board)


move dx dy piece model =
    let
        movedPiece =
            movePiece dx dy piece ( boardWidth, boardHeight )
    in
        if isAllowedMove movedPiece model.board then
            { model | piece = Just movedPiece }
        else
            model


rotate piece model =
    let
        rotatedPiece =
            rotatePiece piece
    in
        if isAllowedMove rotatedPiece model.board then
            { model | piece = Just rotatedPiece }
        else
            model


updateSeed : Random.Seed -> Model -> Model
updateSeed seed model =
    { model | randomSeed = seed }


updatePiecesBag piecesBag model =
    { model | piecesBag = piecesBag }


resetPiecesBag model =
    let
        gen =
            shuffle allPieces

        ( newBag, seed ) =
            Random.step gen model.randomSeed
    in
        model
            |> updateSeed seed
            |> updatePiecesBag newBag


nextPiece : Model -> ( Piece, Model )
nextPiece model =
    let
        gen =
            choose model.piecesBag
    in
        case Random.step gen model.randomSeed of
            ( ( Just piece, pieceBag ), seed ) ->
                ( piece, updatePiecesBag pieceBag model )

            ( ( Nothing, _ ), seed ) ->
                model
                    |> resetPiecesBag
                    |> nextPiece


currentPiece model =
    case model.piece of
        Just p ->
            ( p, model )

        Nothing ->
            nextPiece model


clearBoard : TetrisBoard -> Model -> Model
clearBoard board model =
    let
        newBoard =
            moveBoardLinesDown emptyBlockColor board
    in
        { model | board = newBoard }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            if model.gameOver then
                if code == 32 then
                    -- Space restars the game
                    init
                else
                    ( model, Cmd.none )
            else
                let
                    ( piece, m ) =
                        currentPiece model
                in
                    case code of
                        74 ->
                            -- j
                            ( move 0 1 piece m, Cmd.none )

                        40 ->
                            -- Down arrow
                            ( move 0 1 piece m, Cmd.none )

                        72 ->
                            -- h
                            ( move -1 0 piece m, Cmd.none )

                        37 ->
                            -- Left arrow
                            ( move -1 0 piece m, Cmd.none )

                        76 ->
                            -- l
                            ( move 1 0 piece m, Cmd.none )

                        39 ->
                            -- Right arrow
                            ( move 1 0 piece m, Cmd.none )

                        32 ->
                            ( rotate piece m, Cmd.none )

                        38 ->
                            -- Up arrow
                            ( rotate piece m, Cmd.none )

                        75 ->
                            -- k
                            ( rotate piece m, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        SeedTime t ->
            ( updateSeed (Random.initialSeed (round t)) model, Cmd.none )

        Tick _ ->
            let
                ticks =
                    model.ticks + 1

                levelTicks =
                    levelSpeed model.currentLevel
            in
                if ticks >= levelTicks then
                    { model | ticks = 0 } |> update Drop
                else
                    ( { model | ticks = ticks }, Cmd.none )

        Drop ->
            let
                ( piece, m ) =
                    currentPiece model

                movedPiece =
                    movePiece 0 1 piece ( boardWidth, boardHeight )
            in
                if isAllowedMove movedPiece model.board then
                    ( move 0 1 piece m, Cmd.none )
                else
                    let
                        board =
                            pieceToBoard piece m

                        newLines =
                            countClearLines emptyBlockColor board

                        cleanBoardModel =
                            clearBoard board m

                        ( newPiece, updatedModel ) =
                            nextPiece cleanBoardModel
                    in
                        if isAllowedMove newPiece updatedModel.board then
                            ( { updatedModel | piece = Just newPiece }
                                |> updatePoints newLines
                                |> updateLevel newLines
                            , Cmd.none
                            )
                        else
                            ( { model | gameOver = True }, Cmd.none )



levelSpeed : Int -> Int
levelSpeed level =
    if level <= 5 then
        1000 - ((level - 1) * 200)
    else
        case level of
            6 ->
                100

            7 ->
                50

            8 ->
                40

            9 ->
                30

            10 ->
                20

            _ ->
                10


updatePoints : Int -> Model -> Model
updatePoints nLines model =
    let
        comboMultiplier =
            case nLines of
                1 ->
                    40

                2 ->
                    100

                3 ->
                    300

                _ ->
                    1200
    in
        { model | points = model.points + (nLines * comboMultiplier) }


updateLevel : Int -> Model -> Model
updateLevel nLines model =
    let
        levelLines =
            model.levelLines + nLines
    in
        if levelLines >= 10 then
            { model | levelLines = levelLines - 10, currentLevel = model.currentLevel + 1 }
        else
            { model | levelLines = levelLines }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (\code -> KeyDown code)
        , Time.every Time.millisecond Tick
        ]


colorToString color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"


renderBlock ( columnIndex, lineIndex ) block =
    let
        blockX =
            columnIndex * blockSize + (blockPadding * (columnIndex + 1))

        blockY =
            lineIndex * blockSize + (blockPadding * (lineIndex + 1))
    in
        rect [ x (toString blockX), y (toString blockY), (width (toString blockSize)), (height (toString blockSize)), rx "5", ry "5", fill (colorToString block) ] []


renderLine lineIndex line =
    Array.indexedMap (\columnIndex block -> (renderBlock ( columnIndex, lineIndex ) block)) line


renderBoard board model =
    board
        |> Array.indexedMap
            (\i line ->
                renderLine i line
            )
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat


getUnsafe index array =
    case Array.get index array of
        Just x ->
            x

        Nothing ->
            Debug.crash "Out of bounds array"


getBoardBlock ( x, y ) board =
    let
        row =
            getUnsafe y board
    in
        getUnsafe x row


boardFillBlock ( x, y ) color board =
    let
        row =
            getUnsafe y board

        updatedRow =
            Array.set x color row
    in
        Array.set y updatedRow board


getPieceShape piece =
    Array.fromList piece.shapes
        |> Array.get piece.current_shape
        |> Maybe.withDefault [ [] ]


getPieceCoordinates : Piece -> List ( Int, Int )
getPieceCoordinates piece =
    let
        shape =
            getPieceShape piece
    in
        shape
            |> (List.indexedMap
                    (\shapeY row ->
                        row
                            |> List.indexedMap (\shapeX v -> ( v, ( shapeX + piece.x, shapeY + piece.y ) ))
                    )
               )
            |> List.concat
            |> List.filter (\( v, _ ) -> v == 1)
            |> List.map Tuple.second


pieceToBoard : Piece -> Model -> TetrisBoard
pieceToBoard piece model =
    let
        pieceCoordinates =
            getPieceCoordinates piece
    in
        pieceCoordinates
            |> Array.fromList
            |> Array.foldr (\pos board -> boardFillBlock pos piece.color board)
                model.board


renderScore model =
    div [ class "score " ]
        [ "Points: "
            ++ (toString model.points)
            |> Html.text
        ]


renderLevel model =
    div [ class "level" ]
        [ "Level: "
            ++ (toString model.currentLevel)
            |> Html.text
        ]


renderInstructions model =
    div [ class "instructions" ]
        [ "Use arrow keys to move, space to flip or alternatively use Vim keys. Space to start a new game"
            |> Html.text
        ]


renderGameOver model =
    if model.gameOver then
        [ rect
            [ 100 |> toString |> height
            , boardPixelWidth |> toString |> width
            , boardPixelHeight / 2 - 50 |> toString |> y
            , fill "white"
            ]
            []
        , text_
            [ textAnchor "middle"
            , (boardPixelWidth / 2) |> toString |> x
            , (boardPixelHeight / 2) |> toString |> y
            , fill "black"
            ]
            [ text "GAME OVER" ]
        ]
    else
        []


view : Model -> Html Msg
view model =
    let
        ( piece, m ) =
            currentPiece model

        board =
            pieceToBoard piece m
    in
        div []
            [ (renderScore model)
            , (renderLevel model)
            , svg [ width (toString boardPixelWidth), height (toString boardPixelHeight) ]
                ((renderBoard board model) ++ (renderGameOver model))
            , renderInstructions model
            ]

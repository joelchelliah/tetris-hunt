module Components.Block exposing (..)

import Model exposing (Block, BlockShape(..), BlockState(..), Color(..), Direction(..), Position, TileSpace, config)
import Utils.Position exposing (areValidPositions, diffPositions, getFreeTilePositions, offsetPositions)


getInitialPositions : BlockShape -> Int -> List Position
getInitialPositions shape rotation =
    case ( shape, rotation ) of
        ( Square, _ ) ->
            [ ( 0, -2 ), ( 1, -2 ), ( 0, -1 ), ( 1, -1 ) ]

        ( Line, _ ) ->
            if modBy 2 rotation == 0 then
                [ ( 0, -4 ), ( 0, -3 ), ( 0, -2 ), ( 0, -1 ) ]

            else
                [ ( -1, -3 ), ( 0, -3 ), ( 1, -3 ), ( 2, -3 ) ]

        ( Pyramid, 0 ) ->
            [ ( 0, -3 ), ( -1, -2 ), ( 0, -2 ), ( 1, -2 ) ]

        ( Pyramid, 1 ) ->
            [ ( 0, -3 ), ( 0, -2 ), ( 1, -2 ), ( 0, -1 ) ]

        ( Pyramid, 2 ) ->
            [ ( -1, -2 ), ( 0, -2 ), ( 1, -2 ), ( 0, -1 ) ]

        ( Pyramid, _ ) ->
            [ ( 0, -3 ), ( -1, -2 ), ( 0, -2 ), ( 0, -1 ) ]

        ( LeftFoot, 0 ) ->
            [ ( 1, -3 ), ( 1, -2 ), ( 0, -1 ), ( 1, -1 ) ]

        ( LeftFoot, 1 ) ->
            [ ( -1, -2 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]

        ( LeftFoot, 2 ) ->
            [ ( -1, -3 ), ( 0, -3 ), ( -1, -2 ), ( -1, -1 ) ]

        ( LeftFoot, _ ) ->
            [ ( -1, -2 ), ( 0, -2 ), ( 1, -2 ), ( 1, -1 ) ]

        ( RightFoot, 0 ) ->
            [ ( -1, -3 ), ( -1, -2 ), ( -1, -1 ), ( 0, -1 ) ]

        ( RightFoot, 1 ) ->
            [ ( -1, -2 ), ( 0, -2 ), ( 1, -2 ), ( -1, -1 ) ]

        ( RightFoot, 2 ) ->
            [ ( 0, -3 ), ( 1, -3 ), ( 1, -2 ), ( 1, -1 ) ]

        ( RightFoot, _ ) ->
            [ ( 1, -2 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]

        ( LeftSnake, _ ) ->
            if modBy 2 rotation == 0 then
                [ ( -1, -2 ), ( 0, -2 ), ( 0, -1 ), ( 1, -1 ) ]

            else
                [ ( 0, -3 ), ( -1, -2 ), ( 0, -2 ), ( -1, -1 ) ]

        ( RightSnake, _ ) ->
            if modBy 2 rotation == 0 then
                [ ( 0, -2 ), ( 1, -2 ), ( -1, -1 ), ( 0, -1 ) ]

            else
                [ ( 0, -3 ), ( 0, -2 ), ( 1, -2 ), ( 1, -1 ) ]


getColor : BlockShape -> Color
getColor shape =
    case shape of
        Square ->
            Yellow

        Line ->
            Teal

        Pyramid ->
            Purple

        LeftFoot ->
            Blue

        RightFoot ->
            Orange

        LeftSnake ->
            Red

        RightSnake ->
            Green


isOutOfView : Maybe Block -> Bool
isOutOfView block =
    case block of
        Just { positions } ->
            List.any (\( _, y ) -> y < 0) positions

        Nothing ->
            False


getNextRotation : Block -> Int
getNextRotation { rotation } =
    modBy 4 (rotation + 1)


getNewBlockAfterFall : Block -> Block
getNewBlockAfterFall block =
    let
        newPositions =
            List.map (\( x, y ) -> ( x, y + 1 )) block.positions
    in
    { block | positions = newPositions }


getNewBlockAfterSpin : Block -> Block
getNewBlockAfterSpin ({ positions, rotation, shape } as block) =
    let
        newRotation =
            getNextRotation block

        currentInitialPositions =
            getInitialPositions shape rotation

        newInitialPositions =
            getInitialPositions shape newRotation

        positionOffsets =
            diffPositions positions currentInitialPositions

        newPositions =
            offsetPositions newInitialPositions positionOffsets
    in
    { block | rotation = newRotation, positions = newPositions }


hasState : BlockState -> Maybe Block -> Bool
hasState blockState block =
    case block of
        Nothing ->
            False

        Just { state } ->
            state == blockState


isMoving : Maybe Block -> Bool
isMoving block =
    hasState (Moving Left) block
        || hasState (Moving Right) block
        || hasState (Moving Down) block


isSpinning : Maybe Block -> Bool
isSpinning block =
    hasState Spinning block


init : BlockShape -> Block
init shape =
    let
        toPreviewPosition ( x, y ) =
            ( x + 3, y + 4 )

        positions =
            getInitialPositions shape 0 |> List.map toPreviewPosition
    in
    { positions = positions
    , color = getColor shape
    , shape = shape
    , rotation = 0
    , state = Spinning
    }


updateMovement : TileSpace -> Maybe Block -> Maybe Block
updateMovement tileSpace =
    Maybe.map
        (\block ->
            let
                newBlock =
                    getNewBlockAfterFall block
            in
            if areValidPositions tileSpace newBlock.positions then
                newBlock

            else
                block
        )


updateSpin : Maybe Block -> Maybe Block
updateSpin =
    Maybe.map
        (\block ->
            if block.state == Spinning then
                getNewBlockAfterSpin block

            else
                block
        )


stopSpin : TileSpace -> Maybe Block -> Maybe Block
stopSpin tileSpace =
    Maybe.andThen
        (\({ shape, rotation } as block) ->
            let
                xOffset =
                    -- toFloat + ceiling to get correct center when width is odd number of tiles
                    toFloat (config.gameWidth - 2) / 2 |> ceiling

                toTileSpacePosition ( x, y ) =
                    ( x + xOffset, y + 2 )

                positions =
                    getInitialPositions shape rotation |> List.map toTileSpacePosition
            in
            if areValidPositions tileSpace positions then
                Just { block | state = Moving Left, positions = positions }

            else
                Nothing
        )

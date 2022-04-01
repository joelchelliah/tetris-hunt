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


getNextRotation : Block -> Int
getNextRotation { rotation } =
    modBy 4 (rotation + 1)


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


getNewBlockAfterMove : Bool -> Block -> Block
getNewBlockAfterMove goDown ({ positions, state } as block) =
    let
        flipDirection prevDirection =
            if prevDirection == Right then
                Left

            else
                Right

        newPositions =
            if goDown then
                List.map (\( x, y ) -> ( x, y + 1 )) positions

            else
                case state of
                    Moving Left ->
                        List.map (\( x, y ) -> ( x - 1, y )) positions

                    Moving Right ->
                        List.map (\( x, y ) -> ( x + 1, y )) positions

                    _ ->
                        positions

        newState =
            case state of
                Moving direction ->
                    if goDown then
                        Moving (flipDirection direction)

                    else
                        state

                _ ->
                    state
    in
    { block | positions = newPositions, state = newState }


getNewBlockAfterDrop : Block -> Block
getNewBlockAfterDrop ({ positions } as block) =
    let
        newPositions =
            List.map (\( x, y ) -> ( x, y + 1 )) positions
    in
    { block | positions = newPositions }


hasState : BlockState -> Maybe Block -> Bool
hasState blockState block =
    case block of
        Nothing ->
            False

        Just { state } ->
            state == blockState


isSpinning : Maybe Block -> Bool
isSpinning =
    hasState Spinning


isMoving : Maybe Block -> Bool
isMoving block =
    hasState (Moving Left) block || hasState (Moving Right) block


isDropping : Maybe Block -> Bool
isDropping =
    hasState Dropping


isValid : TileSpace -> Maybe Block -> Bool
isValid tileSpace block =
    case block of
        Nothing ->
            False

        Just { positions } ->
            areValidPositions tileSpace positions


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


updateSpin : Maybe Block -> Maybe Block
updateSpin =
    Maybe.map getNewBlockAfterSpin


startMove : TileSpace -> Maybe Block -> Maybe Block
startMove tileSpace =
    Maybe.andThen
        (\({ shape, rotation } as block) ->
            let
                xOffset =
                    -- toFloat + ceiling to get correct center when width is odd number of tiles
                    toFloat (config.gameWidth - 2) / 2 |> ceiling

                toTileSpacePosition ( x, y ) =
                    ( x + xOffset, y + 3 )

                positions =
                    getInitialPositions shape rotation |> List.map toTileSpacePosition
            in
            if areValidPositions tileSpace positions then
                Just { block | state = Moving Left, positions = positions }

            else
                Nothing
        )


updateMove : TileSpace -> Maybe Block -> Maybe Block
updateMove tileSpace =
    Maybe.map
        (\block ->
            let
                movedBlock =
                    getNewBlockAfterMove False block

                loweredBlock =
                    getNewBlockAfterMove True block
            in
            if areValidPositions tileSpace movedBlock.positions then
                movedBlock

            else if areValidPositions tileSpace loweredBlock.positions then
                loweredBlock

            else
                block
        )


startDrop : Maybe Block -> Maybe Block
startDrop =
    Maybe.map (\block -> { block | state = Dropping })


updateDrop : TileSpace -> Maybe Block -> Maybe Block
updateDrop tileSpace =
    Maybe.map
        (\block ->
            let
                droppedBlock =
                    getNewBlockAfterDrop block
            in
            if areValidPositions tileSpace droppedBlock.positions then
                droppedBlock

            else
                block
        )

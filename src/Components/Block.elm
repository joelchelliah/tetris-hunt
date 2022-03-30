module Components.Block exposing (..)

import Model exposing (Block, BlockShape(..), BlockState(..), Color(..), Position, TileSpace, config)
import Utils.Position exposing (diffPositions, getFreeTilePositions, offsetPositions)


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
            [ ( 0, -2 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]

        ( Pyramid, 1 ) ->
            [ ( 0, -2 ), ( 0, -1 ), ( 1, -1 ), ( 0, 0 ) ]

        ( Pyramid, 2 ) ->
            [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( 0, 0 ) ]

        ( Pyramid, _ ) ->
            [ ( 0, -2 ), ( -1, -1 ), ( 0, -1 ), ( 0, 0 ) ]

        ( LeftFoot, 0 ) ->
            [ ( 0, -3 ), ( 0, -2 ), ( -1, -1 ), ( 0, -1 ) ]

        ( LeftFoot, 1 ) ->
            [ ( -1, -2 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]

        ( LeftFoot, 2 ) ->
            [ ( -1, -3 ), ( 0, -3 ), ( -1, -2 ), ( -1, -1 ) ]

        ( LeftFoot, _ ) ->
            [ ( -1, -2 ), ( 0, -2 ), ( 1, -2 ), ( 1, -1 ) ]

        ( RightFoot, 0 ) ->
            [ ( 0, -3 ), ( 0, -2 ), ( 0, -1 ), ( 1, -1 ) ]

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
                [ ( 0, -2 ), ( -1, -1 ), ( 0, -1 ), ( -1, 0 ) ]

        ( RightSnake, _ ) ->
            if modBy 2 rotation == 0 then
                [ ( 0, -2 ), ( 1, -2 ), ( -1, -1 ), ( 0, -1 ) ]

            else
                [ ( 0, -2 ), ( 0, -1 ), ( 1, -1 ), ( 1, 0 ) ]


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


isValid : TileSpace -> Block -> Bool
isValid tileSpace block =
    let
        isPositionOutOfView ( _, y ) =
            y < 0

        isOnFreeTile pos =
            isPositionOutOfView pos
                || (getFreeTilePositions tileSpace
                        |> List.member pos
                   )
    in
    List.all isOnFreeTile block.positions


getNextRotation : Block -> Int
getNextRotation { rotation } =
    let
        d1 =
            Debug.log "rotation" rotation

        d2 =
            Debug.log "NEW rotation" (modBy 4 (rotation + 1))
    in
    modBy 4 (rotation + 1)


updateAfterFall : Block -> Block
updateAfterFall block =
    let
        newPositions =
            List.map (\( x, y ) -> ( x, y + 1 )) block.positions
    in
    { block | positions = newPositions }


updateAfterRotate : Block -> Block
updateAfterRotate ({ positions, rotation, shape } as block) =
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


init : BlockShape -> Block
init shape =
    let
        centerXOffset =
            -- toFloat + ceiling to get correct center when width is odd
            toFloat (config.gameWidth - 2) / 2 |> ceiling

        positions =
            getInitialPositions shape 0
                |> List.map (\( x, y ) -> ( x + centerXOffset, y ))
    in
    { positions = positions
    , color = getColor shape
    , shape = shape
    , rotation = 0
    , state = Falling
    }


fall : TileSpace -> Maybe Block -> ( Maybe Block, Bool )
fall tileSpace maybeBlock =
    -- Returns a pair of the (potentially updated) block, and a Bool saying whether it fell or not.
    case maybeBlock of
        Nothing ->
            ( Nothing, False )

        Just block ->
            let
                newBlock =
                    updateAfterFall block
            in
            if isValid tileSpace newBlock then
                ( Just newBlock, True )

            else
                ( maybeBlock, False )


rotate : TileSpace -> Maybe Block -> Maybe Block
rotate tileSpace maybeBlock =
    case maybeBlock of
        Nothing ->
            Nothing

        Just block ->
            let
                newBlock =
                    updateAfterRotate block
            in
            if isValid tileSpace newBlock then
                Just newBlock

            else
                maybeBlock

module Components.Block exposing (..)

import Model exposing (Block, BlockShape(..), BlockState(..), Color(..), Position, TileSpace, config)
import Utils.Position exposing (getFreeTilePositions)


centerXOffset =
    (config.gameWidth - 2) // 2


squarePositions : List Position
squarePositions =
    [ ( 0, -2 ), ( 1, -2 ), ( 0, -1 ), ( 1, -1 ) ]


linePositions : List Position
linePositions =
    [ ( 0, -4 ), ( 0, -3 ), ( 0, -2 ), ( 0, -1 ) ]


pyramidPositions : List Position
pyramidPositions =
    [ ( 1, -2 ), ( 0, -1 ), ( 1, -1 ), ( 2, -1 ) ]


getColor : BlockShape -> Color
getColor shape =
    case shape of
        Square ->
            Yellow

        Line ->
            Teal

        Pyramid ->
            Purple

        Hook ->
            Orange

        ReverseHook ->
            Blue

        Snake ->
            Green

        ReverseSnake ->
            Red


getInitialPositions shape =
    let
        center =
            List.map (\( x, y ) -> ( x + centerXOffset, y ))

        positions =
            case shape of
                Square ->
                    squarePositions

                Line ->
                    linePositions

                Pyramid ->
                    pyramidPositions

                _ ->
                    squarePositions
    in
    center positions


isOutOfView : Maybe Block -> Bool
isOutOfView block =
    case block of
        Just { positions } ->
            List.any (\( _, y ) -> y < 0) positions

        Nothing ->
            False


canFall : TileSpace -> Block -> Bool
canFall tileSpace block =
    let
        isNewPositionOutOfView y =
            (y + 1) < 0

        isFreeTileBelow ( x, y ) =
            isNewPositionOutOfView y
                || (getFreeTilePositions tileSpace
                        |> List.member ( x, y + 1 )
                   )
    in
    List.all isFreeTileBelow block.positions


fall : Block -> Block
fall block =
    let
        newPositions =
            List.map (\( x, y ) -> ( x, y + 1 )) block.positions
    in
    { block | positions = newPositions }


init : BlockShape -> Block
init shape =
    { positions = getInitialPositions shape
    , color = getColor shape
    , shape = shape
    , rotation = 1
    , state = Falling
    }


update : TileSpace -> Maybe Block -> ( Maybe Block, Bool )
update tileSpace maybeBlock =
    -- Returns a pair of the (potentially updated) block, and a Bool saying whether it fell or not.
    case maybeBlock of
        Nothing ->
            ( Nothing, False )

        Just block ->
            if canFall tileSpace block then
                ( Just (fall block), True )

            else
                ( maybeBlock, False )

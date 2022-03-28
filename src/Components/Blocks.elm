module Components.Blocks exposing (..)

import Model exposing (Block, BlockState(..), Color(..), Position, TileSpace)
import Utils.Position exposing (getFreeTilePositions)


getSquarePositions : List Position
getSquarePositions =
    [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]


centerAlign : List Position -> List Position
centerAlign =
    List.map (\( x, y ) -> ( x + 5, y + 1 ))


blockSquare : Block
blockSquare =
    { positions = getSquarePositions |> centerAlign
    , rotation = 1
    , color = Blue
    , state = Falling
    }


canFall : TileSpace -> Block -> Bool
canFall tileSpace block =
    let
        isFreeTileBelow ( x, y ) =
            getFreeTilePositions tileSpace |> List.member ( x, y + 1 )
    in
    List.all isFreeTileBelow block.positions


fall : Block -> Block
fall block =
    let
        newPositions =
            List.map (\( x, y ) -> ( x, y + 1 )) block.positions
    in
    { block | positions = newPositions }


init : Maybe Block
init =
    Just blockSquare


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

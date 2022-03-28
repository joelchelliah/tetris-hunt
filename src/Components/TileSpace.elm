module Components.TileSpace exposing (..)

import Model exposing (Block, Tile(..), TileSpace)


updateTileFromBlock : Block -> Tile -> Tile
updateTileFromBlock { color, positions } tile =
    case tile of
        Free pos ->
            if List.member pos positions then
                Locked pos color

            else
                tile

        _ ->
            tile


updateTilesFromBlock : Maybe Block -> TileSpace -> TileSpace
updateTilesFromBlock block tileSpace =
    case block of
        Nothing ->
            tileSpace

        Just newBlock ->
            let
                updateRow =
                    List.map (updateTileFromBlock newBlock)
            in
            List.map updateRow tileSpace


init : Int -> Int -> TileSpace
init width height =
    let
        initTile ( x, y ) =
            if x == 1 || x == width || y == height then
                Wall

            else
                Free ( x, y )

        initRow y =
            List.map (\x -> initTile ( x, y )) (List.range 1 width)
    in
    List.map initRow (List.range 1 height)


update : Maybe Block -> TileSpace -> TileSpace
update block tileSpace =
    let
        newTileSpace =
            updateTilesFromBlock block tileSpace
    in
    -- TODO: Check if there is a valid row and set tiles to Decaying
    -- TODO: Update decaying tiles
    newTileSpace

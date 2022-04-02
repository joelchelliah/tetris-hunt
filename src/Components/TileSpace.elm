module Components.TileSpace exposing (..)

import Model exposing (Block, Tile(..), TileSpace)


updateTileFromBlock : Block -> Tile -> Tile
updateTileFromBlock { color, positions } tile =
    case tile of
        Free position ->
            if List.member position positions then
                Locked { position = position, color = color }

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


markRemovableTiles : TileSpace -> TileSpace
markRemovableTiles =
    let
        isLockedOrWall tile =
            case tile of
                Locked _ ->
                    True

                Wall ->
                    True

                _ ->
                    False

        updateTile tile =
            case tile of
                Locked blockTile ->
                    Decaying blockTile 0

                other ->
                    other

        updateRow row =
            if List.all isLockedOrWall row then
                List.map updateTile row

            else
                row
    in
    List.map updateRow


init : Int -> Int -> TileSpace
init width height =
    let
        initTile ( x, y ) =
            if x == 1 || x == width || y == height then
                Wall

            else
                -- 0-indexing
                Free ( x - 1, y - 1 )

        initRow y =
            List.map (\x -> initTile ( x, y )) (List.range 1 width)
    in
    List.map initRow (List.range 1 height)


update : Maybe Block -> TileSpace -> TileSpace
update block tileSpace =
    let
        newTileSpace =
            updateTilesFromBlock block tileSpace
                |> markRemovableTiles
    in
    -- TODO: Check if there is a valid row and set tiles to Decaying
    -- TODO: Update decaying tiles
    newTileSpace

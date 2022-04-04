module Components.TileSpace exposing (..)

import Model exposing (Block, Color(..), Tile(..), TileSpace)
import Utils.Position exposing (areValidPositions, getTopMostDecayedTilePosition, isHigher)


numDecayTicks : number
numDecayTicks =
    5


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


addTilesFromBlock : Maybe Block -> TileSpace -> TileSpace
addTilesFromBlock block tileSpace =
    case block of
        Nothing ->
            tileSpace

        Just newBlock ->
            let
                updateRow =
                    List.map (updateTileFromBlock newBlock)
            in
            List.map updateRow tileSpace


initFallingTiles : TileSpace -> TileSpace
initFallingTiles tileSpace =
    let
        topMostDecayedPos =
            getTopMostDecayedTilePosition tileSpace

        toFallingTile pos tile =
            case tile of
                Locked blockTile ->
                    if isHigher blockTile.position pos then
                        Falling blockTile

                    else
                        tile

                _ ->
                    tile
    in
    case topMostDecayedPos of
        Nothing ->
            tileSpace

        Just pos ->
            List.map (List.map (toFallingTile pos)) tileSpace


updateFallingTiles : TileSpace -> TileSpace
updateFallingTiles tileSpace =
    let
        updateTile tile =
            case tile of
                Falling blockTile ->
                    Debug.todo "IMPLEMENT THIS!"

                _ ->
                    tile

        updateRow row =
            let
                newRow =
                    List.map updateTile row
            in
            if areValidPositions tileSpace newRow then
                newRow

            else
                row
    in
    List.map updateRow (List.reverse tileSpace)


initDecayingTiles : TileSpace -> TileSpace
initDecayingTiles =
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

                _ ->
                    tile

        updateRow row =
            if List.all isLockedOrWall row then
                List.map updateTile row

            else
                row
    in
    List.map updateRow


updateDecayingTiles : TileSpace -> TileSpace
updateDecayingTiles =
    let
        updateTile tile =
            case tile of
                Decaying blockTile i ->
                    if i < numDecayTicks then
                        Decaying blockTile (i + 1)

                    else
                        Decayed blockTile.position

                _ ->
                    tile
    in
    List.map (List.map updateTile)


clearDecayedTiles : TileSpace -> TileSpace
clearDecayedTiles =
    let
        updateTile tile =
            case tile of
                Decayed pos ->
                    Free pos

                _ ->
                    tile
    in
    List.map (List.map updateTile)


init : Int -> Int -> TileSpace
init width height =
    let
        initTile ( x, y ) =
            if x == 1 || x == width || y == height then
                Wall

            else if y == (height - 1) then
                Locked { position = ( x - 1, y - 1 ), color = Red }

            else
                -- 0-indexing
                Free ( x - 1, y - 1 )

        initRow y =
            List.map (\x -> initTile ( x, y )) (List.range 1 width)
    in
    List.map initRow (List.range 1 height)


update : Maybe Block -> TileSpace -> TileSpace
update block tileSpace =
    addTilesFromBlock block tileSpace
        |> initFallingTiles
        |> updateFallingTiles
        |> initDecayingTiles
        |> updateDecayingTiles
        |> clearDecayedTiles

module Views.Tiles exposing (..)

import Components.Block as Block
import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (class)
import Model exposing (Block, BlockState(..), Color(..), Model, Msg, Tile(..))


type alias Container msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type TileClass
    = FreeTile
    | PreviewTile
    | Tile String


colorToString : Color -> String
colorToString color =
    case color of
        Yellow ->
            "yellow"

        Teal ->
            "teal"

        Purple ->
            "purple"

        Orange ->
            "orange"

        Blue ->
            "blue"

        Green ->
            "green"

        Red ->
            "red"


makeTile : Container msg -> TileClass -> Html msg
makeTile tileContainer tileClass =
    let
        wrapInClass className tile =
            div [ class className ] [ tile ]
    in
    case tileClass of
        FreeTile ->
            tileContainer [] [] |> wrapInClass "background-tile"

        PreviewTile ->
            tileContainer [] [] |> wrapInClass "preview-tile"

        Tile name ->
            tileContainer [ class ("tile " ++ name) ] [] |> wrapInClass "background-tile"


placeWallTile : Html msg
placeWallTile =
    Tile "wall" |> makeTile div


placeFreeTile : Html msg
placeFreeTile =
    makeTile span FreeTile


placePreviewTile : Html msg
placePreviewTile =
    makeTile span PreviewTile


placeBlockTile : Color -> Html msg
placeBlockTile color =
    Tile ("block " ++ colorToString color) |> makeTile div


viewTile : Maybe Block -> Tile -> Html Msg
viewTile block tile =
    case tile of
        Wall ->
            placeWallTile

        Locked _ color ->
            placeBlockTile color

        UnLocked _ color ->
            placeBlockTile color

        Free pos ->
            case block of
                Nothing ->
                    placeFreeTile

                Just { positions, color } ->
                    if List.member pos positions then
                        placeBlockTile color

                    else
                        placeFreeTile

        Removing _ _ _ ->
            -- TODO: Removing animation based on float param
            placeWallTile


previewTile : Maybe Block -> Tile -> Html Msg
previewTile block tile =
    case tile of
        Free pos ->
            case block of
                Nothing ->
                    placePreviewTile

                Just { positions, color } ->
                    if List.member pos positions then
                        placeBlockTile color

                    else
                        placePreviewTile

        _ ->
            div [] []


preview : Model -> Html Msg
preview { previewSpace, block } =
    let
        previewRow row =
            div [ class "row" ] (List.map (previewTile block) row)
    in
    div [ class "preview-container" ]
        [ div [ class "preview" ] (List.map previewRow previewSpace)
        ]


view : Model -> Html Msg
view ({ tileSpace, block } as model) =
    let
        viewRow movingBlock row =
            div [ class "row" ] (List.map (viewTile movingBlock) row)
    in
    if Block.hasState Spinning block then
        div []
            [ preview model
            , div [] (List.map (viewRow Nothing) tileSpace)
            ]

    else
        div [] (List.map (viewRow block) tileSpace)

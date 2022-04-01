module Model exposing (..)


type alias Model =
    { state : GameState
    , block : Maybe Block
    , tileSpace : TileSpace
    , previewSpace : TileSpace
    , frameDeltas : FrameDeltas
    , mouseMoveDebug : MouseMoveData
    }


type alias Block =
    { positions : List Position -- The position of each tile
    , rotation : Int -- Between 0 - 3, representing each rotated state
    , state : BlockState
    , shape : BlockShape
    , color : Color
    }


type alias TileSpace =
    List (List Tile)


type alias FrameDeltas =
    -- To calculate correct time intervals according to the browser's render loop
    { spinTickDelta : Float
    , moveTickDelta : Float
    , dropTickDelta : Float
    }


type alias Position =
    ( Int, Int )


type alias MouseMoveData =
    { offsetX : Int
    , offsetY : Int
    , width : Float
    , height : Float
    }


type Tile
    = Wall
    | Free Position -- Open position without any tiles
    | Locked Position Color -- Block tile, locked into position
    | UnLocked Position Color -- Block tile, unlocked by tile below being removed
    | Removing Position Color Float -- Float for decay animation


type Color
    = Yellow
    | Teal
    | Purple
    | Orange
    | Blue
    | Green
    | Red


type GameState
    = Init
    | Running
    | Paused
    | GameOver


type BlockState
    = Moving Direction
    | Spinning
    | Dropping


type Direction
    = Left
    | Right


type BlockShape
    = Square
    | Line
    | Pyramid
    | LeftFoot
    | RightFoot
    | LeftSnake
    | RightSnake


type Msg
    = MouseMove MouseMoveData
    | MouseClick
    | Enter
    | NewBlock BlockShape
    | FrameDelta Float
    | SpinTick
    | MoveTick
    | DropTick


config =
    { gameWidth = 11 -- Must correspond to $num-tiles-per-row in tiles.scss
    , gameHeight = 15 -- Must correspond to $num-rows in tiles.scss
    , spinTickDelay = 350
    , moveTickDelay = 100
    , dropTickDelay = 10
    }

module Model exposing (..)


type alias Model =
    { state : GameState
    , blocks : List Block
    , tileSpace : TileSpace
    , frameDeltas : FrameDeltas
    , mouseMoveDebug : MouseMoveData
    }


type alias Block =
    { parts : List BlockPart
    , state : BlockState
    }


type alias BlockPart =
    { position : Position
    , color : Color
    }


type alias TileSpace =
    List (List Tile)


type alias FrameDeltas =
    -- To calculate correct time intervals according to the browser's render loop
    { tickDelta : Float }


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
    | Free Position
    | Taken Position Color
    | Removing Position


type Color
    = Green
    | Blue
    | Yellow
    | Pink
    | Teal


type GameState
    = Init
    | Running
    | Paused


type BlockState
    = Flying
    | Falling


type Msg
    = FrameDelta Float
    | Enter
    | Tick
    | KeyDown String
    | MouseMove MouseMoveData
    | Noop


config =
    { gameWidth = 10
    , gameHeight = 14
    , gameSpeed = 90 -- Number of milliseconds between each Tick
    }

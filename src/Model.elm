module Model exposing(..)
import Browser.Dom exposing (getViewport)
import Task
import Svg.Attributes exposing (speed)
import List exposing (range)
import Svg exposing (a)
import Json.Encode exposing (int)
import Random
import Html.Attributes exposing (value)
import Dict exposing (values)
import Json.Decode exposing (Value)
import Svg.Attributes exposing (direction)
import Message exposing (..)




type AnimState =
    Stand 
    | Run 
    | Walk 
    | Jump 

type alias Vector a=
    { x:a
    , y:a
    }

type alias Pos =
    { x1: Float
    , x2: Float
    , y1: Float
    , y2: Float }

type alias Speed = 
    Vector Float

type State
    = Playing

type alias Brick =
    { pos: Pos,
      speed: Speed
    }

type alias Map = 
    { bricks: List Brick
     ,characters: List Player
     ,exit: Pos
    }

type alias Player =
    { pos: Pos
    , anim: AnimState
    , frame: Int
    , direction: MoveDirection
    , speed: Speed
    , hp: Int
    }


type alias Model =
    { player: Player
    , map: Map
    , state: State
    , size: Vector Float
    , audioList: List String
    , attrs: CustomAttribute
    }

type alias CustomAttribute ={ }

attribute =
    { range = Vector 1600 800
    }

init : () -> (Model, Cmd Msg)
init _= 
    ({ player = initPlayer
      ,map = initMap
      ,state = Playing
      ,size = Vector 0 0
      ,audioList = []
      ,attrs = {}
    },Task.perform GetViewport getViewport)

initPlayer =
    { pos = Pos (700+1600) (830+1600) (4000-200) 4000 -- 455 -> 130; 700 ->200
    , anim =Stand
    , frame = 0
    , direction = Left
    , speed = Vector 0 0
    , hp = 1
    }
initCharacter =
    [
    { pos = Pos (700+1600) (900+1600) (4000-200) 4000
    , anim = Run
    , frame = 0
    , hp = 1
    , speed = Vector 0 0
    , xleft = 0
    , xright = 0 
    }
    ]
    

initMap =
    let
        bricks = 
            [ Pos 3135 3200 800 4000
            , Pos 3200 3600 800 900
            , Pos 1600 1700 0 3200
            , Pos 4000 4400 3900 4000
            , Pos 2800 3100 3800 3935 --[1600,3200]*[3200,4000]
            , Pos 2350 2650 3635 3700
            , Pos 1800 2100 3470 3535
            , Pos 2800 3100 3305 3370
            , Pos 2250 2500 3200 3265 --[1600,3200]*[3200,4000]
            , Pos 1700 2000 3035 3100 --[1600,3200]*[2400,3200]
            , Pos 2700 3000 2900 3000
            , Pos 2175 2475 2700 2800
            , Pos 1700 2000 2400 2535 --[1600,3200]*[2400,3200]
            , Pos 2250 2450 2150 2250 --[1600,3200]*[1600,2400]
            , Pos 2600 2900 1950 2050
            , Pos 2100 2600 1600 1850 --[1600,3200]*[1600,2400]
            , Pos 1900 2300 1300 1365 --[1600,3200]*[800,1600]
            , Pos 2500 2900 1300 1365
            , Pos 1900 2300 800 1000
            , Pos 2500 2900 800 1000  --[1600,3200]*[800,1600]
            , Pos 0 4000 4001 4020 
            ] |> List.map (\pos-> {pos = pos, speed = Vector 0 0})
        characters =
            [ Pos 2900 3000 3205 3305
            , Pos 1800 1900 2935 3035
            , Pos 1750 1850 2300 2400
            , Pos 2000 2100 1200 1300
            , Pos 2700 2800 1200 1300
            ] |> List.map (\pos-> { pos = Pos 1200 1400 800 1000
            , anim =Stand
            , frame = 0
            , direction = Left
            , speed = Vector 0 0
            , hp = 1
            })
        exit = Pos 0 0 0 0
    in
        { bricks = bricks
        ,characters = characters
        ,exit = exit
        }


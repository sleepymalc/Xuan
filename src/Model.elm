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
    , collisionPos: List Pos
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
      ,map = initMap1
      ,state = Playing
      ,size = Vector 0 0
      ,audioList = []
      ,attrs = {}
    },Task.perform GetViewport getViewport)

initPlayer =
    { pos = Pos (700+1600) (830+1600) (4000-200) 4000  -- 455 -> 130; 700 ->200
    , collisionPos = standcollisionPos
    , anim =Stand
    , frame = 0
    , direction = Left
    , speed = Vector 0 0
    , hp = 1
    }

standcollisionPos =    
    [ Pos (710+1600) (810+1600) (4000-140) 4000
    , Pos (730+1600) (790+1600) (4000-200) (4000-140) ]
initMap1 =
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
            , collisionPos = [Pos 1200 1400 800 1000]
            , anim =Stand
            , frame = 0
            , direction = Left
            , speed = Vector 0 0
            , hp = 1
            })
        exit = Pos 0 0 0 0
    in
        { bricks = bricks
        , characters = characters
        , exit = exit
        }
initMap2 =
    let
        bricks =
            [ Pos 0 100 0 2000
            , Pos 0 100 2200 4800
            , Pos 100 1500 4735 4800
            , Pos 1500 1600 0 4800
            , Pos 500 800 4600 4735   --[0,1600]*[4000,4800]
            , Pos 100 350 4250 4350
            , Pos 900 1100 4150 4250
            , Pos 1300 1500 4000 4100 --[0,1600]*[4000,4800]
            , Pos 250 500 3835 3900   --[0,1600]*[3200,4000]
            , Pos 1000 1200 3750 3835
            , Pos 650 850 3535 3600
            , Pos 250 700 3200 3300
            , Pos 1100 1400 3300 3400 --[0,1600]*[3200,4000]
            , Pos 100 350 2900 3000   --[0,1600]*[2400,3200]
            , Pos 1250 1500 2935 3000
            , Pos 1400 1500 2670 2735
            , Pos 900 1250 2670 2735
            , Pos 550 750 2600 2670
            , Pos 500 550 2575 2670
            , Pos 250 550 2500 2575   --[0,1600]*[2400,3200]
            , Pos 950 1300 2200 2500  --[0,1600]*[1600,2400]
            , Pos 1200 1300 2100 2200
            , Pos 600 950 1650 2300
            , Pos 100 600 2200 2300
            , Pos 350 600 1900 1965
            , Pos 100 250 1700 1765
            , Pos 600 1250 1550 1650  --[0,1600]*[1600,2400]
            , Pos 250 750 1200 1300   --[0,1600]*[800,1600]
            , Pos 1250 1500 1250 1350
            , Pos 900 1250 950 1050   --[0,1600]*[800,1600]
            , Pos 100 600 700 800     --[0,1600]*[0,800]
            , Pos 400 1200 300 400    --[0,1600]*[0,800]
            ] |> List.map (\pos-> {pos = pos, speed = Vector 0 0})
        characters =
            [ Pos 150 250 4150 4250
            , Pos 300 400 3735 3835
            , Pos 300 400 3100 3200
            , Pos 1200 1300 3200 3300
            , Pos 1300 1400 2835 2935
            , Pos 1000 1100 2570 2670
            , Pos 600 700 2500 2600
            , Pos 1000 1100 2100 2200
            , Pos 150 250 2100 2200
            , Pos 400 500 1800 1900
            , Pos 125 225 1600 1700
            , Pos 650 750 1450 1550
            , Pos 800 900 1450 1550
            , Pos 400 500 1100 1200
            , Pos 425 525 200 300
            , Pos 1075 1175 200 300
            , Pos 750 850 100 300
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
    




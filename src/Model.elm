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
import MapSetting exposing (..)


type AnimState =
    Stand 
    | Run 
    | Walk
    | Crouch
    | Charge
    | Jump 
    | Attack
    | Attacked


type alias Vector a=
    { x:a
    , y:a
    }

type alias Speed = 
    Vector Float

type Stage
    = One
    | Two
    | Three
    | Discover
    | Discovery

type alias Brick =
    { pos: Pos,
      speed: Speed
    }

type alias Map = 
    { bricks: List Brick
     ,characters: List Character
     ,exit: Pos
    }

type alias Player =
    { text: String
    , pos: Pos --
    , collisionPos: List Pos -- 
    , anim: AnimState -- 
    , frame: Int
    , textframe: Int
    , direction: MoveDirection
    , jumpdir: Jump
    , speed: Speed
    , hp: Int
    , chargetime: Float
    }

type alias Character =
    { pos: Pos --
    , collisionPos: List Pos -- 
    , anim: AnimState -- 
    , frame: Int
    , direction: MoveDirection
    , jumpdir: Jump
    , speed: Speed
    , range: Vector Float
    , hp: Int
    , chargetime: Float
    }

type alias Model =
    { player: Player
    , map: Map
    , state: Stage
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
      ,state = One
      ,size = Vector 0 0
      ,audioList = []
      ,attrs = {}
    },Task.perform GetViewport getViewport)

initPlayer =
    { text = "I need to get outta here."
    , pos = Pos 2300 2400 3830 3930
    , collisionPos = standcollisionPos (Pos 2300 2400 3830 3930)
    , anim = Stand
    , frame = 0
    , textframe = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 1
    , chargetime = 0
    }


standcollisionPos pos=   
    [ Pos (pos.x1+35) (pos.x2-35) (pos.y1+4) (pos.y1+35)
    , Pos (pos.x1+15) (pos.x2-15) (pos.y1+35) (pos.y2+4) ]


initCharacters posList= 
    List.map 
        (\pos-> { pos = pos
                , collisionPos = [pos]
                , anim =Walk
                , range = Vector (pos.x1-100) (pos.x2+100)
                , frame = 0
                , direction = Left
                , jumpdir = Up
                , speed = Vector -0.05 0
                , hp = 1
                , chargetime=0
                }) posList

initBricks posList = posList
    |> List.map (\pos-> {pos = pos, speed = Vector 0 0})

initMap1 =
    let
        bricks = initBricks brickPosList1 
        characters = initCharacters characterPosList1 
        exit = Pos 0 0 0 0
    in
        { bricks = bricks
        , characters = characters
        , exit = exit
        }
initMap2 =
    let
        bricks = initBricks brickPosList2
        characters = initCharacters characterPosList2
        exit = Pos 0 100 2000 2200
    in
        { bricks = bricks
        ,characters = characters
        ,exit = exit
        }

initMap3 =
    let
        bricks = initBricks brickPosList3
        characters = initCharacters characterPosList3
        exit = Pos 0 0 0 0
    in
        { bricks = bricks
        ,characters = characters
        ,exit = exit
        }
initMap4 =
    let
        bricks = initBricks brickPosList4
        characters = initCharacters characterPosList4
        exit = Pos 0 0 0 0
    in
        { bricks = bricks
        , exit = exit
        , characters = characters
        }

initMap5 =
    let
        bricks = initBricks brickPosList5
        characters = initCharacters characterPosList5
        exit = Pos 0 0 0 0
    in
        { bricks = bricks
        , exit = exit
        , characters = characters
        }
    




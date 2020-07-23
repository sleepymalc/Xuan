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
    | DiscoverI
    | DiscoverII

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
    ({ player = initPlayer1
      ,map = initMap1
      ,state = One
      ,size = Vector 0 0
      ,audioList = []
      ,attrs = {}
    },Task.perform GetViewport getViewport)

initPlayer1 =
    { text = "I need to get outta here."
    , pos = MapSetting.playerPos1
    , collisionPos = standcollisionPos MapSetting.playerPos1
    , anim = Crouch
    , frame = 0
    , textframe = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 1
    , chargetime = 0
    }


initPlayerDiscoverI =
    { text = "What's going on?"
    , pos = MapSetting.playerPos4
    , collisionPos = standcollisionPos MapSetting.playerPos4
    , anim = Crouch
    , frame = 0
    , textframe = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 1
    , chargetime = 0
    }

initPlayer2 =
    { text = "I am Song Yuanhuai."
    , pos = MapSetting.playerPos2
    , collisionPos = standcollisionPos MapSetting.playerPos2
    , anim = Crouch
    , frame = 0
    , textframe = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 1
    , chargetime = 0
    }

initPlayerDiscoverII =
    { text = "Life is a series of choices, and you don't know about the consequences."
    , pos = MapSetting.playerPos5
    , collisionPos = standcollisionPos MapSetting.playerPos5
    , anim = Crouch
    , frame = 0
    , textframe = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 1
    , chargetime = 0
    }

initPlayer3 =
    { text = "I am back... FOR REVENGE!"
    , pos = MapSetting.playerPos3
    , collisionPos = standcollisionPos MapSetting.playerPos3
    , anim = Crouch
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
    , Pos (pos.x1+15) (pos.x2-15) (pos.y1+35) (pos.y2) ]


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
        { bricks = initBricks brickPosList1 
        , characters = initCharacters characterPosList1 
        , exit = exitPos1
        --exit = Pos 3000 10000 2000 3900
        }
initMap2 =
        { bricks = initBricks brickPosList2
        ,characters = initCharacters characterPosList2
        ,exit = exitPos2
        }

initMap3 =
        { bricks = initBricks brickPosList3
        ,characters = initCharacters characterPosList3
        ,exit = exitPos3
        }
initMapDiscoverI =
        { bricks = initBricks brickPosList4
        , characters = initCharacters characterPosList4
        , exit = exitPos4
        }

initMapDiscoverII =
        { bricks = initBricks brickPosList5
        , characters = initCharacters characterPosList5
        , exit = exitPos5
        }
    




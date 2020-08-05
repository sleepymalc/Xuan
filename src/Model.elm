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
import AISettings exposing (..)
import Load exposing (..)

type alias Story =
    { text: String
    , storyframe: Int
    }

type AnimState =
    Stand 
    | Walk
    | Crouch
    | Charge
    | Jump 
    | Attack
    | Attacked
    | Grovel
    | Fly
    | Dead
    | DebugMode
    | JumpStart
    | JumpEnd
    | JumpLoop
    | Getup

type Mood = 
    Normal
    | Rage


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
    | LOGO
    | Start
    | About
    | End
    | Loading
    | Story1_1
    | Story1_2
    | Story1_3
    | Story1_4 
    | Story1_5   
    | Story2_1
    | Story2_2    
    | Story3_1
    | Story4_Win
    | Story4_Lose
    | Story5_0
    | Story5_1
    | Story5_2
    | Story6_1
    | CG1_1
    | CG1_2
    | CG1_3
    | CG1_4
    | CG2_1
    | CG2_2
    | CG3_1
    | CG5_1
    | CG5_2
    | CG6_1
    | CG6_2

type alias Brick =
    { pos: Pos,
      speed: Speed
    }

type alias Map = 
    { bricks: List Brick
    , wallbricks: List Brick
    , characters: List Character
    , birds: List Bird
    , npcs: List NPC
    , exit: Pos
    }

type alias Player =
    { text: String
    , teachtextstate: Int
    , pos: Pos --
    , collisionPos: List Pos -- 
    , anim: AnimState -- 
    , mood: Mood
    , frame: Int
    , textframe: Int
    , direction: MoveDirection
    , jumpdir: Jump
    , speed: Speed
    , hp: Float
    , chargetime: Float
    , ragetime: Float
    , ragecount: Float
    , inrage: Bool
    , fallcount: Int
    , effecttimeHalf: Float
    , effecttimeOne: Float
    , effecttimeTwo: Float
    , effecttimeThree: Float
    , effecttimeFour: Float
    , effecttimeFive: Float
    }

type alias Character =
    { pos: Pos 
    , collisionPos: List Pos 
    , anim: AnimState 
    , frame: Int
    , direction: MoveDirection
    , jumpdir: Jump
    , speed: Speed
    , range: Vector Float
    , hp: Int
    , chargetime: Float
    , effecttime: Float
    }

type alias NPC = 
    { pos: Pos
    , anim: AnimState
    , frame: Int
    , textframe: Int
    , count: Int
    , text: String
    , direction: MoveDirection
    , speed: Speed
    }

type alias Bird = 
    { pos: Pos
    , anim: AnimState
    , frame: Int
    , direction: MoveDirection
    , speed: Speed
    }

type alias SpeedAI =
    {  pos: Pos 
    , collisionPos: List Pos 
    , anim: AnimState 
    , frame: Int
    , direction: MoveDirection
    , jumpdir: Jump
    , speed: Speed
    , speedAIAnimList: List SpeedAIAnim
    , chargetime: Float
    , hp: Int
    , fallcount: Int
    }
type alias Boss = 
    { pos: Pos 
    , collisionPos: List Pos 
    , anim: AnimState 
    , range: Vector Float
    , frame: Int
    , direction: MoveDirection
    , speed: Speed
    , hp: Int
    }


initSpeedAI = 
    { pos = speedAIPos2
    , collisionPos = standcollisionPos speedAIPos2
    , anim = Stand
    , frame = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , speedAIAnimList = AISettings.initSpeedAIAnimList
    , chargetime = 0
    , hp = 10
    , fallcount = 0
    }

initBoss = 
    { pos = bossPos3 
    , collisionPos = standcollisionPos bossPos3
    , anim = Walk
    , range = bossRange
    , frame = 0
    , direction = Left
    , speed = Vector -0.05 0
    , hp = 3
    }
bossRange = Vector 650 950

type alias Model =
    { player: Player
    , map: Map
    , state: Stage
    , size: Vector Float
    , audioList: List String
    , attrs: CustomAttribute
    , time: Float
    , story: Story
    , cgtime: Float
    , loadPack: List String
    , speedAI: SpeedAI
    , record: List SpeedAIAnim
    , boss: Boss
    }

type alias CustomAttribute ={ }

attribute =
    { range = Vector 1600 800
    }

init : () -> (Model, Cmd Msg)
init _= 
    ({ player = initPlayer1
      ,map = initMap1
      ,state = Loading
      ,size = Vector 0 0
      ,audioList = []
      ,attrs = {}
      ,time = 0
      ,story = initstory
      ,cgtime = 5000
      ,loadPack = initLoadPack
      ,speedAI = initSpeedAI
      ,record = []
      ,boss = initBoss
    },Cmd.batch 
        [ Task.perform GetViewport getViewport ])


initstory =
    { text = ""
    , storyframe = 0
    }


initPlayer1 =
    { text = "I need to get outta here."
    , teachtextstate = 0
    , pos = MapSetting.playerPos1
    , collisionPos = standcollisionPos MapSetting.playerPos1
    , anim = Crouch
    , mood = Normal
    , frame = 0
    , textframe = -1000
    , direction = Right
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 10
    , chargetime = 0
    , ragetime = 0
    , ragecount = 0
    , inrage = False
    , fallcount = 0
    , effecttimeOne = 0
    , effecttimeTwo = 200
    , effecttimeThree = 400
    , effecttimeFour = 600
    , effecttimeFive = 800
    , effecttimeHalf = 0
    }


initPlayerDiscoverI player=
    { player |
      text = "What's going on?"
    , pos = MapSetting.playerPosDiscoverI
    , collisionPos = standcollisionPos MapSetting.playerPosDiscoverI
    , anim = JumpEnd
    , frame = 0
    , textframe = 0
    , direction = Right
    , jumpdir = Up
    , speed = Vector 0 0 
    , fallcount = 0
    }

initPlayer2 player =
    { player |
      text = "I am Song Yuanhuai."
    , pos = MapSetting.playerPos2
    , collisionPos = standcollisionPos MapSetting.playerPos2 
    , anim = Getup
    , frame = 0
    , textframe = 0
    , direction = Right
    , jumpdir = Up
    , speed = Vector 0 0 
    , fallcount = 0
    }

initPlayerDiscoverII player =
    { player |
      text = "Life is a series of choices, and you don't know about the consequences."
    , pos = MapSetting.playerPosDiscoverII
    , collisionPos = standcollisionPos MapSetting.playerPosDiscoverII
    , anim = Crouch
    , frame = 0
    , textframe = 0
    , direction = Right
    , jumpdir = Up
    , speed = Vector 0 0 
    , fallcount = 0
    }

initPlayer3 player=
    { player |
      text = "I am back... FOR REVENGE!"
    , pos = MapSetting.playerPos3
    , collisionPos = standcollisionPos MapSetting.playerPos3
    , anim = Crouch
    , frame = 0
    , textframe = 0
    , direction = Right
    , jumpdir = Up
    , speed = Vector 0 0
    , fallcount = 0
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
                , chargetime = 0
                , effecttime = 0
                }) posList


initBricks posList = posList
    |> List.map (\pos-> {pos = pos, speed = Vector 0 0})

initMap1 =
        { bricks = initBricks brickPosList1
        , wallbricks = initBricks brickWallList1
        , characters = initCharacters characterPosList1 
        , exit = exitPos1
        , npcs = initNpcs1 npcPosList1
        , birds = initBirds1 birdPosList1
        }
        
initMapDiscoverI =
        { bricks = initBricks brickPosListDiscoverI
        , wallbricks = initBricks brickWallListDiscoverI
        , characters = initCharacters characterPosListDiscoverI
        , exit = exitPosDiscoverI
        , npcs = initNpcsDiscoverI npcPosListDiscoverI
        , birds = initBirdsDiscoverI birdPosListDiscoverI
        }

initMap2 =
        { bricks = initBricks brickPosList2
        , wallbricks = initBricks brickWallList2
        , characters = initCharacters characterPosList2
        , exit = exitPos2
        , npcs = initNpcs2 npcPosList2
        , birds = initBirds2 birdPosList2
        }

initMapDiscoverII =
        { bricks = initBricks brickPosListDiscoverII
        , wallbricks = initBricks brickWallListDiscoverII
        , characters = initCharacters characterPosListDiscoverII
        , exit = exitPosDiscoverII
        , npcs = initNpcsDiscoverII npcPosListDiscoverII
        , birds = initBirdsDiscoverII birdPosListDiscoverII
        }

initMap3 =
        { bricks = initBricks brickPosList3
        , wallbricks = initBricks brickWallList3
        , characters = initCharacters characterPosList3
        , exit = exitPos3
        , npcs = initNpcs3 npcPosList3
        , birds = initBirds3 birdPosList3
        }        

initNpcs1 posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
                , count = 0
                , text = "Go UP!!! ESCAPE!!! By the way, nice to see you!"
                , direction = Left
                , speed = Vector 0 0
                , textframe = 0
        }) posList

initNpcsDiscoverI posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = -500
                , count = 0
                , text = "Be careful, your power is very strong now..."
                , direction = Left
                , speed = Vector 0 0
                , textframe = 0
        }) posList

initNpcs2 posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
                , count = 0
                , text = "Go HIGH, Go FAST! Don't let XUAN takeover your body!!!"
                , direction = Left
                , speed = Vector 0 0
                , textframe = 0
        }) posList

initNpcsDiscoverII posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
                , count = 0
                , text = "Hey, where are you going?"
                , direction = Left
                , speed = Vector 0 0
                , textframe = 0
        }) posList

initNpcs3 posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
                , count = 0
                , text = "REVENGE?? Interesting..."
                , direction = Left
                , speed = Vector 0 0
                , textframe = 0
        }) posList

initBirds1 posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
                , direction = Right
                , speed = Vector 0 0
        }) posList

initBirdsDiscoverI posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
                , direction = Right
                , speed = Vector 0 0
        }) posList

initBirds2 posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
                , direction = Right
                , speed = Vector 0 0
        }) posList

initBirdsDiscoverII posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
                , direction = Right
                , speed = Vector 0 0
        }) posList
        
initBirds3 posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
                , direction = Right
                , speed = Vector 0 0
        }) posList
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
    | DebugMode

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
    | StoryOne
    | StoryTwo
    | StoryThree
    | StoryFour
    | StoryFive
    | StorySix
    | Loading
    

type alias Brick =
    { pos: Pos,
      speed: Speed
    }

type alias Map = 
    { bricks: List Brick
     ,characters: List Character
     ,birds: List Bird
     ,npcs: List NPC
     ,exit: Pos
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
    }

type alias NPC = 
    { pos: Pos
    , anim: AnimState
    , frame: Int
    , count: Int
    , text: String
    }

type alias Bird = 
    { pos: Pos
    , anim: AnimState
    , frame: Int
    }

-- for jump jumpdir time
-- for walk walkdir pos



type alias SpeedAI =
    {  pos: Pos 
    , collisionPos: List Pos 
    , anim: AnimState 
    , frame: Int
    , direction: MoveDirection
    , jumpdir: Jump
    , speed: Speed
    , speedAIAnimList : List SpeedAIAnim
    , chargetime : Float
    , hp : Int
    }

initSpeedAI = 
    { pos = speedAIPos3
    , collisionPos = standcollisionPos speedAIPos3
    , anim = Stand
    , frame = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , speedAIAnimList = AISettings.initSpeedAIAnimList
    , chargetime = 0
    , hp = 10
    }

type alias Model =
    { player: Player
    , map: Map
    , state: Stage
    , size: Vector Float
    , audioList: List String
    , attrs: CustomAttribute
    , time: Float
    , story: Story
    , loadPack: List String
    , speedAI: SpeedAI
    , record: List SpeedAIAnim
    }

type alias CustomAttribute ={ }

attribute =
    { range = Vector 1600 800
    }

init : () -> (Model, Cmd Msg)
init _= 
    ({ player = initPlayer3
      ,map = initMap3
      ,state = Two
      ,size = Vector 0 0
      ,audioList = []
      ,attrs = {}
      ,time = 0
      ,story = initstory
      ,loadPack = initLoadPack
      ,speedAI = initSpeedAI
      ,record = []
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
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 10
    , chargetime = 0
    , ragetime = 0
    
    }


initPlayerDiscoverI =
    { text = "What's going on?"
    , teachtextstate = -1
    , pos = MapSetting.playerPos4
    , collisionPos = standcollisionPos MapSetting.playerPos4
    , anim = Crouch
    , mood = Normal
    , frame = 0
    , textframe = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 10
    , chargetime = 0
    , ragetime = 0
    
    }

initPlayer2 =
    { text = "I am back... FOR REVENGE!I am Song Yuanhuai."
    , teachtextstate = -1
    , pos = MapSetting.playerPos2
    , collisionPos = standcollisionPos MapSetting.playerPos2
    , anim = Crouch
    , mood = Normal
    , frame = 0
    , textframe = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 10
    , chargetime = 0
    , ragetime = 0
    
    }

initPlayerDiscoverII =
    { text = "Life is a series of choices, and you don't know about the consequences."
    , teachtextstate = -1
    , pos = MapSetting.playerPos5
    , collisionPos = standcollisionPos MapSetting.playerPos5
    , anim = Crouch
    , mood = Normal
    , frame = 0
    , textframe = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 10
    , chargetime = 0
    , ragetime = 0
    
    }

initPlayer3 =
    { text = "I am Song Yuanhuai."
    , teachtextstate = -1
    , pos = MapSetting.playerPos3
    , collisionPos = standcollisionPos MapSetting.playerPos3
    , anim = Crouch
    , mood = Normal
    , frame = 0
    , textframe = 0
    , direction = Left
    , jumpdir = Up
    , speed = Vector 0 0
    , hp = 10
    , chargetime = 0
    , ragetime = 0
    
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

initNpcs posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
                , count = 0
                , text = "Nice to see you!"
        }) posList

initBirds posList=
    List.map
        (\pos-> { pos = pos
                , anim = Stand
                , frame = 0
        }) posList

initBricks posList = posList
    |> List.map (\pos-> {pos = pos, speed = Vector 0 0})

initMap1 =
        { bricks = initBricks brickPosList1 
        , characters = initCharacters characterPosList1 
        , exit = exitPos1
        , npcs = initNpcs npcPosList1
        , birds = initBirds birdPosList1
        }
initMap2 =
        { bricks = initBricks brickPosList2
        ,characters = initCharacters characterPosList2
        ,exit = exitPos2
        , npcs = initNpcs npcPosList2
        , birds = initBirds birdPosList2
        }

initMap3 =
        { bricks = initBricks brickPosList3
        ,characters = initCharacters characterPosList3
        ,exit = exitPos3
        , npcs = initNpcs npcPosList3
        , birds = initBirds birdPosList3
        }
initMapDiscoverI =
        { bricks = initBricks brickPosList4
        , characters = initCharacters characterPosList4
        , exit = exitPos4
        , npcs = initNpcs npcPosList4
        , birds = initBirds birdPosList4
        }

initMapDiscoverII =
        { bricks = initBricks brickPosList5
        , characters = initCharacters characterPosList5
        , exit = exitPos5
        , npcs = initNpcs npcPosList5
        , birds = initBirds birdPosList5
        }
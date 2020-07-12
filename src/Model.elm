module Model exposing(..)
import Message exposing (..)
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
    { pos: Pos
      --speed: Speed
    }

type alias Map = 
    { bricks: List Brick
     ,characters: List Character
     ,exit: Pos
    }
type alias Character = 
    { pos: Pos
    , anim: AnimState
    , hp: Int
    , speed: Speed
    , xleft: Int
    , xright: Int
    }
type alias Player =
    { pos: Pos,
      anim: AnimState,
      speed: Speed,
      hp: Int
    }

type AnimState =
    Run Int


type alias Model =
    { player: Player
    , character: List Character
    , map: List Map
    , state: State
    , size: Vector Float
    , audioList: List String
    , attrs: CustomAttribute
    }

type alias CustomAttribute ={ }

attribute =
    { range = Vector 1600 1000
    }

init : () -> (Model, Cmd Msg)
init _= 
    ({ player = initPlayer
      ,character = initCharacter
      ,map = []
      ,state = Playing
      ,size = Vector 0 0
      ,audioList = []
      ,attrs = {}
    },Task.perform GetViewport getViewport)

initPlayer =
    { pos = Pos 1200 1400 800 1000
     ,anim = Run 0
     ,speed = Vector 0 0
     ,hp = 1
    }
initCharacter =
    [
    { pos = Pos 1200 1400 800 1000 
    , anim = Run 0
    , hp = 1
    , speed = Vector 0 0
    , xleft = 0
    , xright = 0 
    }
    ]


initMap =
    [ initMap1
    , initMap2
    , initMap3
    ]

initMap1 =
    let
        bricks = 
            [ Pos 1 3 4 5
            , Pos 1 3 4 5
            ]
        characters = []
        exit = Pos 0 0 0 0
    in
        { bricks = bricks
        ,characters = characters
        ,exit = exit
        }
initMap2 =
    let
        bricks = 
            [ Pos 1 3 4 5
            , Pos 1 3 4 5
            ]
        characters = []
        exit = Pos 0 0 0 0
    in
        { bricks = bricks
        ,characters = characters
        ,exit = exit
        }
initMap3 =
    let
        bricks = 
            [ Pos 1 3 4 5
            , Pos 1 3 4 5
            ]
        characters = []
        exit = Pos 0 0 0 0
    in
        { bricks = bricks
        ,characters = characters
        ,exit = exit
        }
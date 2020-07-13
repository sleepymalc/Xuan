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
import Svg.Attributes exposing (direction)

type alias Vector =
    { x: Float
    , y: Float }

type alias Pos =
    { x1: Float
    , x2: Float
    , y1: Float
    , y2: Float }

type alias Speed = 
    Vector

type State
    = Playing

type alias Brick =
    { pos: Pos,
      speed: Speed
    }


type alias Player =
    { pos: Pos
    , anim: AnimState
    , frame: Int
    , direction: MoveDirection
    , speed: Speed
    }
type AnimState =
    Stand 
    | Run 
    | Walk 
    | Jump 


type alias Model =
    { player: Player
    , state: State
    , size: Vector
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
    , state = Playing
    , size = Vector 0 0
    , audioList = []
    , attrs = {}
    },Task.perform GetViewport getViewport)

initPlayer =
    { pos = Pos 1200 1400 800 1000,
      anim =Stand,
      frame = 0,
      direction = Left,
      speed = Vector 0 0
    }

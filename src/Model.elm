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
    { pos: Pos,
      speed: Speed
    }

type alias Map = 
    { bricks: List Brick
     ,characters: List Player
     ,exit: Pos
    }

type alias Player =
    { pos: Pos,
      anim: AnimState,
      speed: Speed
    }

type AnimState =
    Run Int


type alias Model =
    { player: Player
    , map: Map
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
      ,map = initMap
      ,state = Playing
      ,size = Vector 0 0
      ,audioList = []
      ,attrs = {}
    },Task.perform GetViewport getViewport)

initPlayer =
    { pos = Pos 1200 1400 800 1000,
      anim = Run 0,
      speed = Vector 0 0
    }

initMap =
    { bricks = []
     ,characters = []
     ,exit = Pos 0 0 0 0
    }
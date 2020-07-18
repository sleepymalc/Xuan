module Message exposing (Msg(..),MoveDirection(..))
import Browser.Dom exposing (Viewport)


type MoveDirection = Left | Right


type Msg
    = AnimWalk MoveDirection Bool
    | AnimJump Bool
    | Resize Int Int
    | Tick Float
    | Noop
    | GetViewport Viewport
    
module Message exposing (Msg(..),MoveDirection(..))
import Browser.Dom exposing (Viewport)

type MoveDirection = Left | Right

type Msg
    = Move MoveDirection Bool
    | Jump Bool
    | Resize Int Int
    | Tick Float
    | Noop
    | GetViewport Viewport
    
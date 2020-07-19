module Message exposing (..)
import Browser.Dom exposing (Viewport)


type MoveDirection = Left | Right

type Jump = L | Up | R

type Msg
    = AnimWalk MoveDirection Bool
    --| AnimJump Jump Bool
    | AnimAttack Bool
    | AnimCharge Bool
    | Resize Int Int
    | Tick Float
    | Noop
    | GetViewport Viewport
    
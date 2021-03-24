module Message exposing (..)
import Browser.Dom exposing (Viewport)



type MoveDirection = Left | Right

type Jump = L | Up | R

type Msg
    = AnimWalk MoveDirection Bool
    | AnimAttack Bool
    | AnimCharge Bool
    | Resize Int Int
    | Tick Float
    | Noop
    | GetViewport Viewport
    | ImageError String
    | ImageLoaded String
    | Start
    | Back
    | Next
    | About
    | DebugPos
    | ExitDebugMode
    
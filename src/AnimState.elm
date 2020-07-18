module AnimState exposing(..)
import Model exposing (..)
import Message exposing (..)


stand player = 
    { player| anim = Stand, frame = 0, speed = Vector 0 0 }

jump player =
    case player.direction of
        Left ->
            { player| anim = Jump, frame = 0, speed = Vector -0.1 -0.2 }
        Right ->
            { player| anim = Jump, frame = 0, speed = Vector 0.1 -0.2 }

walk moveDirection player = 
    if player.anim == Stand then
        case moveDirection of
            Left ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector -0.1 0 }
            Right ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector 0.1 0 }
    else player

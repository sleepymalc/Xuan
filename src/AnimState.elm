module AnimState exposing(..)
import Model exposing (..)
import Message exposing (..)


stand player = 
    { player| anim = Stand, frame = 0, speed = Vector 0 0 }

jump player =
    case player.direction of
        Left ->
            { player| anim = Jump, frame = 0, speed = Vector (-0.001*player.chargetime) (-0.001*player.chargetime) }
        Right ->
            { player| anim = Jump, frame = 0, speed = Vector (0.001*player.chargetime) (-0.001*player.chargetime) }

walk moveDirection player = 
    if player.anim == Stand then
        case moveDirection of
            Left ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector -0.2 0 }
            Right ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector 0.2 0 }
    else player

attack player =
    if player.anim == Stand then
        { player| anim = Attack, frame = 0, speed = Vector 0 0 }
    else player
charge player =
    {player | anim = Charge}

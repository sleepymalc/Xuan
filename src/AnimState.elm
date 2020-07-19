module AnimState exposing(..)
import Model exposing (..)
import Message exposing (..)


crouch player =
    { player| anim = Crouch, frame = 0  }

fall player =
    { player| anim = Stand, frame = 0, speed = Vector 0 0 }

attacked player =
    { player| anim = Stand, frame = 0, speed = Vector 0 0 }

jumpdirection moveDirection player =
    case moveDirection of
        Left ->
            { player| jumpdir = L}
        Right ->
            { player| jumpdir = R}

stand player = 
    { player| anim = Stand, frame = 0, speed = Vector 0 0, jumpdir = Up }

jump player =
    case player.jumpdir of
        L ->
            { player| anim = Jump, frame = 0, speed = Vector (-0.001*player.chargetime) (-0.001*player.chargetime) }
        R ->
            { player| anim = Jump, frame = 0, speed = Vector (0.001*player.chargetime) (-0.001*player.chargetime) }
        Up ->
            { player| anim = Jump, frame = 0, speed = Vector 0 (-0.001*player.chargetime) }

walk moveDirection player = 
    if player.anim == Stand then
        case moveDirection of
            Left ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector -0.2 0, jumpdir = L }
            Right ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector 0.2 0, jumpdir = R }

    else player

attack player =
    if player.anim == Stand then
        { player| anim = Attack, frame = 0, speed = Vector 0 0 }
    else player
charge player =
    {player | anim = Charge}

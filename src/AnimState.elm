module AnimState exposing(..)
import Model exposing (..)
import Message exposing (..)
import Model exposing (AnimState(..))


crouch player =
    { player| anim = Crouch, frame = 0  }

fall player =
    { player| anim = Stand, frame = 0, speed = Vector 0 0 }

attacked speed player =
    { player| anim = Attacked, frame = 0, speed = speed}

jumpdirection moveDirection player =
    case moveDirection of
        Left ->
            { player| jumpdir = L}
        Right ->
            { player| jumpdir = R}

stand player = 
    { player| anim = Stand, frame = 0, speed = Vector 0 0, jumpdir = Up }

jump player =
    let
        time = if player.chargetime > 2500 then
                2500
            else
                player.chargetime
    in
        case player.jumpdir of
            L ->
               { player| anim = Jump, frame = 0, speed = Vector (-0.0005*time) (-0.00075*time) }
               --{ player| anim = Jump, frame = 0, speed = Vector (-0.75) (-(time*time-2500*time+0.49999999)) }
            R ->
                { player| anim = Jump, frame = 0, speed = Vector (0.0005*time) (-0.00075*time) }
            Up ->
                { player| anim = Jump, frame = 0, speed = Vector 0 (-0.001*time) }

walk moveDirection player = 
    if player.anim == Stand then
        case moveDirection of
            Left ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector -0.2 0, jumpdir = L }
            Right ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector 0.2 0, jumpdir = R }

    else player

attack player =
    if player.anim == Stand || player.anim == Walk then
        { player| anim = Attack, frame = 0, speed = Vector 0 0 }
    else player
charge player =
    {player | anim = Charge}

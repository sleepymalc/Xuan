module AnimState exposing(..)
import Model exposing (..)
import Message exposing (..)
import Model exposing (AnimState(..))

-- boss Attacked Stand
-- jumpdir
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
    { player| anim = Stand, frame = 0, speed = Vector 0 0}

releaseJumpdir player =
    { player| jumpdir = Up }

jump player =
    let
        time = if player.chargetime > 2000 then
                2
            else
                player.chargetime/1000
    in
        case player.jumpdir of
            L ->
               { player| anim = Jump, frame = 0, speed = Vector (-0.5) (0.25*time^3-0.75*time^2-0.25)}
            R ->
               { player| anim = Jump, frame = 0, speed = Vector 0.5 (0.25*time^3-0.75*time^2-0.25) }
            Up ->
                { player| anim = Jump, frame = 0, speed = Vector 0 (0.25*time^3-0.75*time^2-0.25)}

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
    {player | anim = Charge, frame = 0}

grovel player = 
    { player | anim = Grovel, frame = 0, speed = Vector 0 0 }

rage player = 
    { player | mood = Rage, frame = 0, speed = Vector 0 0}

normal player = 
    { player | mood = Normal, frame = 0, speed = Vector 0 0}

dead player =
    { player| anim = Dead,  frame = 0, speed = Vector 0 0}

jumpStart player=
    { player| anim = JumpStart,  frame = 0, speed = Vector 0.01 -0.01}

jumpLoop
    player=
    { player| anim = JumpLoop,  frame = 0 }
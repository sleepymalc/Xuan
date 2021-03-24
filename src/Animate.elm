module Animate exposing (..)

import Model exposing (..)
import AnimState exposing(..)
import Collision exposing (..)
import Text exposing (..)
import Message exposing (..)
import MapSetting exposing (..)
import AISettings exposing (..)
import Html exposing (th)
import ChangeState exposing (..)



animate time model =
    let
        speedAI = if model.state == Two then
                model.speedAI 
                |> moveSpeedAI model.time
                |> changeChargeTime 17
                |> changeAnim (model.map.bricks++model.map.wallbricks) 17
                |> changeSpeed 17 model (model.map.bricks++model.map.wallbricks)
                |> touchDown 17 (model.map.bricks++ model.map.wallbricks)
                |> changePos 17
                |> changeFrame 17
            else
                model.speedAI
    
        player = 
            if model.player.anim == DebugMode then
                model.player
                |> changePos time
            else
                model.player
                |> rage 
                |> health
                |> chargeEffectTime time
                |> changeRageTime time
                |> attackedByCharacters model.map.characters
                |> attackedByCharacter boss
                |> changeChargeTime time
                |> changeAnim (model.map.bricks++ model.map.wallbricks) time
                |> changeSpeed time model (model.map.bricks++ model.map.wallbricks)
                |> touchDown time (model.map.bricks++ model.map.wallbricks)
                |> changeTextframe time
                |> changeText model.state model.speedAI
                |> cleartext
                |> changePos time
                |> changeFrame time
                |> stageAnim time model

        boss = if model.state == Three then
                model.boss
                -- |> towardsPlayer model.player
                |> bossAttackPlayer model.player
                |> bossChangeAnim
                |> bossAttackedByPlayer model.player
                |> tour time
                |> changePos time
                |> changeFrame time
                |> debugBoss 
                else 
                    model.boss

        characters = List.filter (\character->attackedByPlayer player character == False) model.map.characters
            |> List.map (\character-> character
            |> attackPlayer model.player
            |> changeAnim (model.map.bricks++ model.map.wallbricks) time
            |> tour time
            |> changePos time
            |> changeFrame time)

        npcs=model.map.npcs
            |> List.map (\npc-> npc
            |> count model.player
            |> changeTextframe time
            |> changeNPCText model)

        map = model.map
            |> changeCharactersAndNpcs characters npcs

    in
        { model | map = map, player = player,  speedAI = speedAI, boss = boss}
            |> chargeModeltime
            |> changeState
            |> changeCGandStory time

stageAnim time model player= 
    let
        playerPos = nextPos player.speed time player.pos

    in
        if (model.state == One) && (player.anim/= JumpStart)&&(rightImpact player.speed time [jumpPos1] playerPos) then
            player |> jumpStart
        else if (model.state == Two) && (rightImpact player.speed time [collidePos2] playerPos) then
            player |> jumpLoop
        else if (player.anim == JumpStart) && (player.frame >= 14*5) then 
            player |> jumpLoop
        else 
            player

debugBoss boss= 
    if boss.anim == Attacked then
        Debug.log "boss" boss
    else
        boss

bossChangeAnim boss =
    if ((boss.anim == Attack) && (boss.frame >=120) )
        || ((boss.anim == Attacked) && (boss.frame >=350)) then
        Debug.log "turn" (boss |> turn)
    else if (boss.anim /= Dead) && (boss.hp <=0) then
        boss |> dead
    else 
        boss

bossAttackedByPlayer player boss = 
    if boss.anim == Attacked then
        boss
    else 
        if (attackedByPlayer player boss) then
            Debug.log "attacked" (boss |> attacked (Vector 0 0))
            |> loseBlood 1
        else
            boss

moveSpeedAI time speedAI =
    case List.head speedAI.speedAIAnimList of
        Just anim->
            if anim.time > time then
                speedAI
            else
                let
                    speedAIAnimList = List.drop 1 speedAI.speedAIAnimList
                    newSpeedAI = {speedAI| speedAIAnimList = speedAIAnimList}
                in
                case anim.msg of
                    AIWalk moveDirection on->
                        if on then
                            newSpeedAI |> walk moveDirection
                        else 
                            newSpeedAI |> stand 
                        
                    AICharge jumpdir on->
                            if on then
                                newSpeedAI |> charge 
                            else 
                                { newSpeedAI| jumpdir = jumpdir}|> jump          
        Nothing ->
            speedAI

chargeModeltime model =
    let
        newtime = model.time + 17
    in
    { model | time = newtime}

changeCharactersAndNpcs characters npcs map =
    { map |characters = characters, npcs = npcs}




attackedByPlayer player character =
    let
        attackPos = playerAttackRange player
    in
        player.anim == Attack 
        && List.any 
                (\pos->projectionOverlap .x1 .x2 attackPos pos&& projectionOverlap .y1 .y2 attackPos pos) 
                character.collisionPos

playerAttackRange player=
    let
        pos = player.pos
        dx = if player.direction == Left then
                -80
            else
                80
    in
        {pos| x1 = pos.x1 + dx, x2 = pos.x2 + dx }

tour time character = 
    let
        pos = nextPos character.speed time character.pos
    in
        if  character.range.x < pos.x1 
            && pos.x2 < character.range.y
        then
            if character.anim == Stand && character.frame >=200 then
                character|> turn
            else
                character
        else character|> stand

turn character =
    let
        (direction, speed) = if character.direction ==Left then
                (Right, Vector 0.05 0)
            else 
                (Left, Vector -0.05 0)
    in
        {character| anim = Walk, speed = speed, direction = direction}

attackPlayer player character = 
    let
        attackPos = attackRange character
    in 
        if character.anim == Attack && character.frame >=30 then
            character |> stand
        else if List.any 
                    (\pos->projectionOverlap .x1 .x2 attackPos pos&& projectionOverlap .y1 .y2 attackPos pos) 
                    player.collisionPos then
            if character.anim == Stand && character.frame <200 then
                character 
            else
                character |> attack
        else
            character


bossForwardAttackRange boss =
    let
        pos = boss.pos
        dx = if boss.direction == Left then
                -0.05 * 3000
            else
                0.05 * 3000
        y1 = (pos.y1+pos.y2)/2
    in
        {pos| x1 = pos.x1 + dx, x2 = pos.x2 + dx, y1 = y1}

bossBackwardAttackRange boss =
    let
        pos = boss.pos
        dx = if boss.direction == Left then
                0.05 * 3000
            else
                -0.05 * 3000
        y1 = (pos.y1+pos.y2)/2
    in
        {pos| x1 = pos.x1 + dx, x2 = pos.x2 + dx, y1 = y1}

bossAttackPlayer player boss = 
    let
        attackPos = bossForwardAttackRange boss 
    in 
        if (player.anim /= Attacked) && ( boss.anim /= Attack )
        && (List.any 
                    (\pos->projectionOverlap .x1 .x2 attackPos pos&& projectionOverlap .y1 .y2 attackPos pos) 
                    player.collisionPos) then
            boss |> attack
        else
            boss |> bossBackAttackPlayer player

bossBackAttackPlayer player boss = 
    let
        attackPos = bossBackwardAttackRange boss
    in 
        if ( boss.anim /= Attacked ) && ( boss.anim /= Attack )
            && (List.any 
                    (\pos->projectionOverlap .x1 .x2 attackPos pos&& projectionOverlap .y1 .y2 attackPos pos) 
                    player.collisionPos) then
                boss |> turn |> attack
        else
            boss


attackedByCharacters characters player=
    List.foldl attackedByCharacter player characters

attackedByCharacter character player =
    let
        attackPos = attackRange character
    in 
        if character.anim == Attack then
           if character.direction == Left then
                player |> attacked (Vector -0.2 0)
                       |> loseBlood 0.08
            else
                player |> attacked (Vector 0.2 0)
                       |> loseBlood 0.08
        else
            player


attackRange character =
    let
        pos = character.pos
        dx = character.speed.x * 2000
    in
        {pos| x1 = pos.x1 + dx, x2 = pos.x2 + dx }

changeChargeTime time player = 
    let 
        newchargetime = player.chargetime + time
    in
    if player.anim == Charge then
        {player|chargetime=newchargetime}
    else    
        {player | chargetime=0}    

chargeEffectTime time player = 
    let
        newEffectTimeHalf = if player.effecttimeOne <=1000 then 
                                player.effecttimeOne + 0.2*time
                            else 
                                -(player.effecttimeOne + 0.2*time) 
        newEffectTimeOne = if player.effecttimeOne <=1000 then 
                                player.effecttimeOne + time
                            else 
                                -(player.effecttimeOne + time)
        newEffectTimeTwo = if player.effecttimeTwo <=1000 then
                                player.effecttimeTwo+ 1.25*time
                            else 
                                -(player.effecttimeTwo + 1.25*time)
        newEffectTimeThree = if player.effecttimeThree <= 1000 then 
                                player.effecttimeThree+ 1.5*time
                            else 
                                -(player.effecttimeThree + 1.5*time)
        newEffectTimeFour = if player.effecttimeFour <= 1000 then
                                player.effecttimeFour+ 1.75*time
                            else    
                                -(player.effecttimeFour + 1.75*time)
        newEffectTimeFive = if player.effecttimeFive <=1000 then 
                                player.effecttimeFive+ 2*time
                            else
                                -(player.effecttimeFive + 2*time)
    in
    { player | 
        effecttimeHalf = newEffectTimeHalf, effecttimeOne = newEffectTimeOne,effecttimeTwo = newEffectTimeTwo,     
        effecttimeThree = newEffectTimeThree, effecttimeFour = newEffectTimeFour, effecttimeFive = newEffectTimeFive} 
        

changeAnim bricks time player=
    let
        playerPos = List.map (nextPos player.speed time) player.collisionPos 
        posList = List.map .pos bricks
        newplayer={player | chargetime=0}
    in
        if player.anim == Charge then
            player        
        else if player.anim == Grovel then
            player |> grovel
        else if player.anim == Jump && player.chargetime > 0 then
            newplayer |> jump
        else if (player.anim == Attack && player.frame >= 30)
            || (player.anim == Crouch && player.frame >= 60)
            || (player.anim == JumpEnd && player.frame >= 7*7)
            || (player.anim == Getup && player.frame >= 280)
            || (player.anim == Attacked && player.frame >= 60) then
            player |> stand 
        else if player.anim == Walk && player.speed.y /=0 && List.any (downImpact player.speed time posList) player.collisionPos == False then
            { newplayer | anim = Jump}
        else newplayer

count player npc = 
    let
        newNPCCount = npc.count+1
    in
    if player.anim == Grovel && abs(player.pos.y1-npc.pos.y1) <400 && npc.count+1 == player.fallcount then
        { npc | count=newNPCCount}
    else
        npc

loseBlood damage player = 
    let 
        hp = player.hp - damage
    in
    { player | hp = hp }

health player = 
    let
        hp=if player.hp>=10 then
            10
            else
            player.hp+0.002        
    in
    { player | hp=hp }

rage player = 
    let 
        ragecount = player.ragecount + 1
    in
    if player.hp <= 0 && player.mood /= Rage then
        { player | mood=Rage, ragecount = ragecount }
    else
        player

changeRageTime time player = 
    let
        newragetime = player.ragetime + time
        newplayer = normal player
        punishtime = if player.inrage then
                        7500 * player.ragecount 
                    else
                        5000 * player.ragecount
    in
    if player.ragetime < punishtime && player.mood == Rage then
        { player | ragetime = newragetime }
    else if player.ragetime > punishtime then
        { newplayer | ragetime = 0, hp = 10 - 1 * player.ragecount}
    else 
        player


changeSpeed time model bricks player =
    let
        playerPos = List.map (nextPos player.speed time) player.collisionPos 
        posList = List.map .pos bricks
        dx = if player.anim == JumpStart then
                0.001
            else if player.anim == JumpLoop 
            && (projectionOverlap .y1 .y2 player.pos deceleratePos) then
                -0.5* player.speed.x
            else if List.any (rightImpact player.speed time posList) playerPos ||
                List.any (leftImpact player.speed time posList) playerPos then
                if player.anim == Walk || player.anim == Attacked then
                    -player.speed.x
                else
                     -1.8 * player.speed.x
            else
                0
        dy = if player.anim == JumpStart then
                -0.001
            else if player.anim == JumpLoop 
            && (projectionOverlap .y1 .y2 player.pos deceleratePos) then
                if model.state == One then
                    -0.014* player.speed.y
                else -0.06* player.speed.y
            else if player.anim == JumpLoop then
                0.01
            else if List.any (upImpact player.speed time posList) playerPos then
                -1.8* player.speed.y
            else 
                0.03
        speed = Vector (player.speed.x + dx) ( player.speed.y + dy) 
    in
        {player | speed = speed}

touchDown time bricks player =
    let
        brickSpeed = Vector -player.speed.x -player.speed.y
        posList = bricks
                    |> List.map .pos 
                    |> List.map (nextPos brickSpeed time)
    in
        List.foldl (touchDownBrick brickSpeed time) player posList

touchDownBrick brickSpeed time brickPos player =    
    if upImpact brickSpeed time player.collisionPos brickPos then
        let 
            dy = brickPos.y1 - time * brickSpeed.y - player.pos.y2
            speed = Vector player.speed.x 0
            pos = Pos player.pos.x1 player.pos.x2 (player.pos.y1 + dy) (player.pos.y2 + dy)
            collisionPos = standcollisionPos pos
            fallcount = player.fallcount+1
            newplayer = loseBlood 1 player
        in
            if player.anim == Jump then
                if player.speed.y >= 1.8 then
                    { newplayer | pos = pos , collisionPos = collisionPos, fallcount = fallcount} |> grovel
                else 
                    { player | pos = pos, collisionPos = collisionPos} |> stand |> releaseJumpdir
            else
                {player | speed = speed, pos =pos , collisionPos = collisionPos}
    else
        player

changePos time player =
    let
        pos = nextPos player.speed time player.pos
        collisionPos = List.map (nextPos player.speed time) player.collisionPos
    in
        {player | pos = pos , collisionPos = collisionPos}

nextPos speed time pos=
    let
        dx = speed.x * time
        dy = speed.y * time 
    in
        Pos (pos.x1 + dx) (pos.x2 + dx) (pos.y1 + dy) (pos.y2 + dy)

changeFrame time player =
    {player | frame = player.frame + 1}

changeTextframe time player =
    { player | textframe = player.textframe + 1 }
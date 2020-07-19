module Animate exposing (..)

import Model exposing (..)
import AnimState exposing(..)
import Collision exposing (..)
import Text exposing (..)
import Message exposing (..)


animate time model =
    let
        player = model.player
            |> attackedByCharacters model.map.characters            
            |> changeChargeTime time
            |> changeAnim model.map.bricks time
            |> changeSpeed time model.map.bricks
            |> touchdown time model.map.bricks
            |> changeTextframe time
            |> changeText model.state
            |> cleartext
            |> changePos time
            |> changeFrame time

        characters = List.filter (\character->attackedByPlayer player character == False) model.map.characters
            |>List.map (\character-> character
            |> attackPlayer model.player
            |> attackedByCharacters model.map.characters
            |> changeAnim model.map.bricks time
            |> tour time
            |> changePos time
            |> changeFrame time)

        map = model.map
        newMap = {map |characters = characters}
    in
        { model| player = player, map = newMap}


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
        --if character.speed 
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

attackedByCharacters characters player=
    List.foldl attackedByCharacter player characters

attackedByCharacter character player =
    let
        attackPos = attackRange character
    in 
        if character.anim == Attack then
           if character.direction == Left then
                player |> attacked (Vector -0.2 0)
            else
                player |> attacked (Vector 0.2 0)
        else
            player


attackRange character =
    let
        pos = character.pos
        dx = character.speed.x * 2000
    in
        --if character.speed 
        {pos| x1 = pos.x1 + dx, x2 = pos.x2 + dx }

changeChargeTime time player = 
    let 
        newchargetime = player.chargetime + time
    in
    if player.anim == Charge then
        {player|chargetime=newchargetime}
    else    
        {player | chargetime=0}    

changeAnim bricks time player=
    let
        posList = List.map .pos bricks
        newplayer={player | chargetime=0}
    in
        if player.anim == Charge then
            player
        else if player.anim == Jump && player.chargetime > 0 then
            newplayer |> jump
        else if player.anim == Jump && List.any (downImpact player.speed time posList) player.collisionPos 
            || (player.anim == Attack && player.frame >= 30)
            || (player.anim == Crouch && player.frame >= 60)
            || (player.anim == Attacked && player.frame >= 60) then
            player |> stand
        else newplayer

changeSpeed time bricks player =
    let
        posList = List.map .pos bricks
        dx = if List.any (rightImpact player.speed time posList) player.collisionPos 
                || List.any (leftImpact player.speed time posList) player.collisionPos then
                if player.anim == Walk then
                    -player.speed.x
                else -1.8 * player.speed.x
            else
                0
        dy = if List.any (upImpact player.speed time posList) player.collisionPos then
                -1.8* player.speed.y
            else 
                0.03

        speed = Vector (player.speed.x + dx) ( player.speed.y + dy) 
    in
        {player | speed = speed}

touchdown time bricks player =
    let
        posList = List.map .pos bricks
        dy = if List.any (downImpact player.speed time posList) player.collisionPos  then
                    -player.speed.y
            else 0
        speed = Vector player.speed.x (player.speed.y + dy) 
    in
        {player | speed = speed}

changePos time player =
    let
        pos = nextPos player.speed time player.pos
        collisionPos = List.map (nextPos player.speed time) player.collisionPos
    in
        {player | pos = pos
                , collisionPos = collisionPos}

nextPos speed time pos=
    let
        dx = speed.x * time 
        
        dy = speed.y * time 
    in
        Pos (pos.x1 + dx) (pos.x2 + dx)
            (pos.y1 + dy) (pos.y2 + dy)

changeFrame time player =
    {player | frame = player.frame + 1}

changeTextframe time player =
    { player | textframe = player.textframe +1 }
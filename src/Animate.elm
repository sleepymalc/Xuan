module Animate exposing (..)

import Model exposing (..)
import AnimState exposing(..)
import Collision exposing (..)
import Text exposing (..)
import Message exposing (..)
import MapSetting exposing (..)
import Model exposing (Stage(..))


animate time model =
    let
        player = model.player
            |> rage 
            |> changeRageTime time
            |> attackedByCharacters model.map.characters            
            |> changeChargeTime time
            |> changeAnim model.map.bricks time
            |> changeSpeed time model.map.bricks
            |> touchDown time model.map.bricks
            |> changeTextframe time
            |> changeText model.state
            |> cleartext
            |> changePos time
            |> changeFrame time

        characters = List.filter (\character->attackedByPlayer player character == False) model.map.characters
            |> List.map (\character-> character
            |> attackPlayer model.player
            |> changeAnim model.map.bricks time
            |> tour time
            |> changePos time
            |> changeFrame time)

        map = model.map
            |> changeCharacters characters

        story = model.story
            |> changeStoryframe time
            |> changeStory model.state
    in
        { model | map = map, player = player, story = story }
            |> changeState
            |> storyEnd

changeState model =
    if arriveExit model then 
        case model.state of
            One -> 
                { model | map = initMapDiscoverI, state = DiscoverI, player = initPlayerDiscoverI}
            DiscoverI ->
                { model | map = initMap3, state = Two, player = initPlayer3}
            Two ->
                { model | map = initMapDiscoverII, state = DiscoverII, player = initPlayerDiscoverII}
            DiscoverII ->
                { model | map = initMap2, state = Three, player = initPlayer2}
            Three ->
                { model | map = initMap1, state = One, player = initPlayer1}
            _ ->
                model
    else
        model
    
storyOneFrame = 200

storyEnd model = 
    case model.state of
        StoryOne ->
            if model.frame == storyOneFrame then
                { model | map = initMap1, state = One, player = initPlayer1, frame = 0}
            else { model | frame = model.frame + 1 }
        _ -> model


changeCharacters characters map =
    {map |characters = characters}


arriveExit model =
    model.player.pos.x2 >= model.map.exit.x1 && model.player.pos.x1 <= model.map.exit.x2
 && model.player.pos.y1 <= model.map.exit.y2 && model.player.pos.y2 >= model.map.exit.y1

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
            || (player.anim == Attacked && player.frame >= 60) then
            player |> stand
        else if player.anim == Walk && player.speed.y /=0 && List.any (downImpact player.speed time posList) player.collisionPos == False then
            { newplayer | anim = Jump}
        else newplayer

loseBlood damage player = 
    let 
        hp = player.hp - damage
    in
    { player | hp = hp }

rage player = 
    if player.hp <= 0 then
        { player | mood=Rage }
    else
        player

changeRageTime time player = 
    let
        newragetime = player.ragetime + time
        newplayer = normal player
    in
    if player.ragetime <= 25000 && player.mood== Rage then
        { player | ragetime = newragetime }
    else if player.ragetime >= 25000 then
        { newplayer | ragetime = 0, hp = 10}
    else 
        player


changeSpeed time bricks player =
    let
        playerPos = List.map (nextPos player.speed time) player.collisionPos 
        posList = List.map .pos bricks
        dx = if List.any (rightImpact player.speed time posList) playerPos 
                || List.any (leftImpact player.speed time posList) playerPos then
                if player.anim == Walk || player.anim == Attacked then
                    -player.speed.x
                else -1.8 * player.speed.x
            else
                0
        dy = if List.any (upImpact player.speed time posList) playerPos then
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
            newplayer = loseBlood 1 player
        in
            if player.anim == Jump then
                if player.speed.y >= 2 then
                    { newplayer | pos = pos , collisionPos = collisionPos} |> grovel
                else 
                    { player | pos = pos, collisionPos = collisionPos} |> stand
            else
                {player | speed = speed, pos =pos , collisionPos = collisionPos}
    else
        player

        

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
    { player | textframe = player.textframe + 1 }

changeStoryframe time story =
    { story | storyframe = story.storyframe + 1 }
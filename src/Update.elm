module Update exposing (..)
import Message exposing (..)
import Model exposing (..)
import Random
import Svg exposing (animate)
import Html exposing (time)
import Svg.Attributes exposing (direction)
import AnimState exposing(..)
import Collision exposing (..)
import Text exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetViewport { viewport } ->
            ( { model
                | size = Vector viewport.width viewport.height
              }
            , Cmd.none
            )
            
        Resize width height -> 
            ( { model 
                | size = Vector (toFloat width) (toFloat height )
                }
            , Cmd.none
            )
        
        AnimWalk moveDirection on->
            if on && model.player.anim == Stand then
                ({model|
                    player=model.player |> walk moveDirection
                },Cmd.none)
            else if not on && model.player.anim /= Jump then
                ({model|
                    player=model.player |> stand
                },Cmd.none)
            else 
                (model,Cmd.none)

        AnimCharge on->
            if on && model.player.anim == Stand then
                ({model|
                    player=model.player |> charge }, Cmd.none )
            else if not on && model.player.anim == Charge then
                ({model| 
                    player=model.player |> jump }, Cmd.none)
            else    
                (model,Cmd.none)

        Tick time -> 
            ( model |> animate time, Cmd.none )

        _ ->
            ( model, Cmd.none )


animate time model =
    let
        player = model.player            
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
        { model| player = player}


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
        else if player.anim == Jump && List.any (downImpact player.speed time posList) player.collisionPos then--||onWall map time player then
            player |> stand
        else newplayer



changeSpeed time bricks player =
    let
        posList = List.map .pos bricks
        dx = if List.any (rightImpact player.speed time posList) player.collisionPos 
                || List.any (leftImpact player.speed time posList) player.collisionPos then
                if player.anim == Walk then
                    -player.speed.x
                else 
                    -1.8 * player.speed.x
            else
                0
        dy = if List.any (upImpact player.speed time posList) player.collisionPos then
                -1.8* player.speed.y
            else 
                0.01

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


-- Todo: 

-- hitted hit()
-- jump speed && xuli contorl 

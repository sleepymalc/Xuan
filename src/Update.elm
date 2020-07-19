module Update exposing (..)
import Message exposing (..)
import Model exposing (..)
import Random
import Svg exposing (animate)
import Html exposing (time)
import Svg.Attributes exposing (direction)
import AnimState exposing(..)
import Collision exposing (..)

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
            case model.state of
                Playing ->
                    ( model |> animate time, Cmd.none )

        AnimAttack on->
            ( {model |
                player=model.player |> AnimState.attack}, Cmd.none )

        _ ->
            ( model, Cmd.none )


animate time model =
    let
        player = model.player            
            |> changeChargeTime time
            |> changeAnim model.map.bricks time
            |> changeSpeed time model.map.bricks
            |> touchdown time model.map.bricks 
            |> changePos time
            |> changeFrame time
        characters = List.map
            (\character-> character
            |> changePos time
            |> changeFrame time) model.map.characters

        map = model.map
        newMap = {map |characters = characters}
    in
        { model| player = player, map = newMap}


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
            || (player.anim == Attack && player.frame >= 50)
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


-- Todo: 

-- hitted hit()
-- jump speed && xuli contorl 

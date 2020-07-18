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
            if on then
                ({model|
                    player=model.player |> walk moveDirection
                },Cmd.none)
            else
                ({model|
                    player=model.player |> stand
                },Cmd.none)


        AnimJump on ->
            ( {model |
                player=model.player |> jump}, Cmd.none )

        Tick time ->
            case model.state of
                Playing ->
                    ( model |> animate time, Cmd.none )

        _ ->
            ( model, Cmd.none )


<<<<<<< HEAD
run moveDirection player = 
    case moveDirection of
        Left ->
            { player| anim = Run, frame = 0, direction= moveDirection, speed = Vector -1 0 }
        Right ->
            { player| anim = Run, frame = 0, direction= moveDirection, speed = Vector 1 0 }

stand player = 
    { player| anim = Stand, frame = 0, speed = Vector 0 0 }

jump player =
    case player.direction of
        Left ->
            { player| anim = Jump, frame = 0, speed = Vector -0.3 -0.45 }
        Right ->
            { player| anim = Jump, frame = 0, speed = Vector 0.3 -0.45 }

walk moveDirection player = 
    if player.anim == Stand then
        case moveDirection of
            Left ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector -0.2 0 }
            Right ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector 0.2 0 }
    else player

=======
>>>>>>> 12f0e28d59c38bafec1b3fdb9350ed0302d40ffb
animate time model =
    let
        player = model.player
            |> changeAnim model.map.bricks time
            |> changeSpeed time model.map.bricks
            |> touchdown time model.map.bricks 
            |> changePos time
            |> changeFrame time
    in
        { model| player = player}



changeAnim bricks time player=
    let
        posList = List.map .pos bricks
    in
        if player.anim == Jump && List.any (downImpact player.speed time posList) player.collisionPos then--||onWall map time player then
            player |> stand
        else player


changeSpeed time bricks player =
    let
        posList = List.map .pos bricks
<<<<<<< HEAD
    in
        ((player.speed.x * time + player.pos.x1) < minX 
                || (player.speed.x * time + player.pos.x2-100) > maxX)
        ||(touchBricks player time posList)

touchBricks player time posList =
    (posList|> List.filter (\pos -> (pos.y1<=player.pos.y1+100) && (player.pos.y2-100<=pos.y2))
            |> List.filter (\pos -> ((player.pos.x1+40>pos.x2) 
                                    && (player.speed.x * time + player.pos.x1+40<pos.x2))
                                    ||((player.pos.x2-120<pos.x1) 
                                    && (player.speed.x * time + player.pos.x2-120>pos.x1)))
            |> List.isEmpty) == False

changeSpeed time map player =
    let
        dx = if  onWall map time player  then
                if player.anim == Walk then
                    -1*player.speed.x
                else 
                    -2 * player.speed.x
=======
        dx = if List.any (rightImpact player.speed time posList) player.collisionPos 
                || List.any (leftImpact player.speed time posList) player.collisionPos then
                -2 * player.speed.x
>>>>>>> 12f0e28d59c38bafec1b3fdb9350ed0302d40ffb
            else
                0
        dy = if List.any (upImpact player.speed time posList) player.collisionPos then
                    -2* player.speed.y
            else 
<<<<<<< HEAD
                    0.001 * time
=======
                    0.0002 * time

>>>>>>> 12f0e28d59c38bafec1b3fdb9350ed0302d40ffb
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
-- walk and jump collision
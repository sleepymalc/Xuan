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
    if player.anim == Jump && downImpact player time (List.map .pos bricks) then--||onWall map time player then
        player |> stand
    else player


changeSpeed time bricks player =
    let
        posList = List.map .pos bricks
        dx = if rightImpact player time posList || leftImpact player time posList then
                -2 * player.speed.x
            else
                0
        dy = if upImpact player time posList then
                    -2* player.speed.y
            else 
                    0.0002 * time

        speed = Vector (player.speed.x + dx) ( player.speed.y + dy) 
    in
        {player | speed = speed}

touchdown time bricks player =
    let
        posList = List.map .pos bricks
        dy = if downImpact player time posList  then
                    -player.speed.y
            else 0
        speed = Vector player.speed.x (player.speed.y + dy) 
    in
        {player | speed = speed}

changePos time player =
    let
        dx = player.speed.x * time 
        
        dy = player.speed.y * time 
        pos = Pos (player.pos.x1 + dx) (player.pos.x2 + dx) 
            ( player.pos.y1 + dy) (player.pos.y2 + dy)
    in
        {player | pos = pos}

changeFrame time player =
    {player | frame = player.frame + 1}


-- Todo: 

-- hitted hit()
-- jump speed && xuli contorl 
-- walk and jump collision
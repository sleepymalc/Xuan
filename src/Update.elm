module Update exposing (..)
import Message exposing (..)
import Model exposing (..)
import Random
import Svg exposing (animate)
import Html exposing (time)

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
            case on of
               True -> 
                    ({model|
                        player=model.player |> walk moveDirection
                    },Cmd.none)
               False -> 
                    ({model|
                        player=model.player 
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
moving brick1 brick2 =
        if brick1.speed.y /= 0 then
            if brick1.speed.y >0 then
                if brick1.speed.y+brick1.pos.y1>brick2.pos.y2 then
                    {brick1|speed = Vector brick1.speed.x -brick1.speed.y, pos = Pos brick1.pos.x1 brick1.pos.x2 (brick1.pos.y1+brick1.speed.y) (brick1.pos.y2+brick1.speed.y)}
                else
                    {brick1|pos = Pos brick1.pos.x1 brick1.pos.x2 (brick1.pos.y1+brick1.speed.y) (brick1.pos.y2+brick1.speed.y)}
            else
                
            if brick1.speed.x+brick1.pos.x1<brick2.pos.x2 || brick1.speed.x+brick1.pos.x2>brick2.pos.x1 then
                {brick1|speed = Vector -brick1.speed.x brick1.speed.y, pos = Pos (brick1.pos.x1+brick1.speed.x) (brick1.pos.x2+brick1.speed.x) brick1.pos.y1 brick1.pos.y2}
            else 
                {brick1|pos = Pos (brick1.pos.x1+brick1.speed.x) (brick1.pos.x2+brick1.speed.x) brick1.pos.y1 brick1.pos.y2}
        else 
            {brick1|pos = Pos (brick1.pos.x1+brick1.speed.x) (brick1.pos.x2+brick1.speed.x) brick1.pos.y1 brick1.pos.y2}
        


run moveDirection player = 
    case moveDirection of
        Left ->
            { player| anim = Run, frame = 0, direction= moveDirection, speed = Vector -0.5 0 }
        Right ->
            { player| anim = Run, frame = 0, direction= moveDirection, speed = Vector 0.5 0 }

stand player = 
    { player| anim = Stand, frame = 0, speed = Vector 0 0 }

jump player =
    case player.direction of
        Left ->
            { player| anim = Jump, frame = 0, speed = Vector -0.1 -0.2 }
        Right ->
            { player| anim = Jump, frame = 0, speed = Vector 0.1 -0.2 }

walk moveDirection player = 
    case moveDirection of
        Left ->
            { player| anim = Walk, frame = 0, direction= moveDirection, speed = Vector -0.1 0 }
        Right ->
            { player| anim = Walk, frame = 0, direction= moveDirection, speed = Vector 0.1 0 }

animate time model =
    let
        player = model.player
            |> changePos time
            
            |> changeSpeed time

            |> changeAnim
            
            |> changeFrame time
    in
        { model| player = player}

onFloor player= player.pos.y1 >= 800


changeAnim player=
    if player.anim == Jump && (player |> onFloor) then
        player |> stand
    else player



changeSpeed time player =
    let
        dx = 0
        dy = if player |> onFloor then
                -player.speed.y/2
            else 
                0.0002 * time
        speed = Vector (player.speed.x + dx) ( player.speed.y + dy) 
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

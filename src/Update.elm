module Update exposing (..)
import Message exposing (..)
import Model exposing (..)
import Random
import Svg exposing (animate)
import Html exposing (time)
import Svg.Attributes exposing (direction)

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
    if player.anim == Stand then
        case moveDirection of
            Left ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector -0.1 0 }
            Right ->
                { player| anim = Walk,  direction= moveDirection, speed = Vector 0.1 0 }
    else player

animate time model =
    let
        player = model.player
            |> changeAnim model.map.bricks time
            |> changeSpeed time model.map.bricks
            |> changePos time
            --|> changeAnim model.map.bricks time
            |> changeFrame time
    in
        { model| player = player}

onFloor bricks time player= 
    let
        posList = List.map .pos bricks
    in
        (((player.pos.y2 + player.speed.y*time)>(4000)))
        ||(onBricks player time posList)

underFloor bricks time player =
    let
        posList = List.map .pos bricks
    in
        underBricks player time posList
onBricks player time posList =
    (posList|> List.filter (\pos -> (pos.x1<=player.pos.x1+80) && (player.pos.x2-100<=pos.x2))
            |> List.filter (\pos -> (player.pos.y2-45<pos.y1) && ((player.pos.y2-45 + 10)>pos.y1))
            |> List.isEmpty) == False

underBricks player time posList =
    (posList|> List.filter (\pos -> (pos.x1<=player.pos.x1+80) && (player.pos.x2-100<=pos.x2))
            |> List.filter (\pos -> (player.pos.y1+40<pos.y2) && ((player.pos.y1 + 50)>pos.y2))
            |> List.isEmpty) == False

changeAnim map time player=
    if (player.anim == Jump && (player |> onFloor map time) && player.speed.y>0) then--||onWall map time player then
        player |> stand
    else player

onWall bricks time player= 
    let
        minX = toFloat(floor (player.pos.x1/1600) * 1600)
        maxX = minX + 1600
        posList = List.map .pos bricks
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
    
        dx = if onWall map time player then
                -2 * player.speed.x
            else
                0
        dy = if (player |> underFloor map time) && player.speed.y < 0 then
                    -2* player.speed.y
            else if (player |> onFloor map time) && player.speed.y > 0  then
                    -player.speed.y
            else if (player |> onFloor map time) && player.speed.y <= 0 then
                    0
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

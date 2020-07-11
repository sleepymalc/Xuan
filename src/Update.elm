module Update exposing (..)
import Message exposing (..)
import Model exposing (..)
import Random

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

        Move moveDirection on->
            case moveDirection of
                Left -> 
                    ({model|
                        player=model.player |> run 
                    },Cmd.none)
                Right -> 
                    ({model|
                        player=model.player |> run 
                    },Cmd.none)

        Tick time ->
            case model.state of
                Playing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )
moving brick1 brick2 =
        if brick1.speed.y != 0 then
            if brick1.speed.y >0 then
                if brick1.speed.y+brick1.pos.y1>brick2.pos.y2 then
                    {brick1|speed = Vector brick1.speed.x -brick1.speed.y, pos = brick1.pos.x1 brick1.pos.x2 brick1.pos.y1+brick1.speed.y brick1.pos.y2+brick1.speed.y}
                else
                    {brick1|pos = brick1.pos.x1 brick1.pos.x2 brick1.pos.y1+brick1.speed.y brick1.pos.y2+brick1.speed.y}
            else
                
            if brick1.speed.x+brick1.pos.x1<brick2.pos.x2 || brick1.speed.x+brick1.pos.x2>brick2.pos.x1 then
                {brick1|speed = Vector -brick1.speed.x brick1.speed.y, pos = brick1.pos.x1+brick1.speed.x brick1.pos.x2+brick1.speed.x brick1.pos.y1 brick1.pos.y2}
            else 
                {brick1|pos = brick1.pos.x1+brick1.speed.x brick1.pos.x2+brick1.speed.x brick1.pos.y1 brick1.pos.y2}
        else 
            {brick1|pos = brick1.pos.x1+brick1.speed.x brick1.pos.x2+brick1.speed.x}
        


run player = player
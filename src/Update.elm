module Update exposing (..)

import Message exposing (..)
import Model exposing (..)
import AnimState exposing(..)
import Collision exposing (..)
import Text exposing (..)
import Animate exposing (..)
import MapSetting exposing (..)


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
            else if on && model.player.anim ==Charge then
                ({model|
                    player=model.player |> jumpdirection moveDirection
                },Cmd.none)
            else if not on && model.player.anim /= Jump && model.player.speed.y == 0 then
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

        AnimAttack on->
            ( {model |
                player=model.player |> AnimState.attack}, Cmd.none )

        _ ->
            ( model, Cmd.none )

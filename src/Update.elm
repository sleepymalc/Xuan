module Update exposing (..)

import Message exposing (..)
import Model exposing (..)
import AnimState exposing(..)
import Collision exposing (..)
import Text exposing (..)
import Animate exposing (..)
import AISettings exposing (..)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetViewport { viewport } ->
            ( { model
                | size = Vector viewport.width viewport.height
              }
            , Cmd.none
            )

        ImageLoaded url ->
            let
                loadPack = List.filter (\loadingurl-> loadingurl/=url) model.loadPack
            in
                if List.isEmpty loadPack then
                    ( { model
                            | state = LOGO
                            , cgtime = 5000
                    }
                    , Cmd.none
                    )
                else 
                    ( { model
                            | loadPack = loadPack
                    }
                    , Cmd.none
                    )
        
        ImageError url ->
            let
                loadPack = List.filter (\loadingurl-> loadingurl/=url) model.loadPack
            in
                if List.isEmpty loadPack then
                    ( { model
                            | state = LOGO
                            , cgtime = 5000
                    }
                    , Cmd.none
                    )
                else 
                    ( { model
                            | loadPack = loadPack
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
                        , record = model.record ++ [Debug.log "msg" { time = model.time, msg = AIWalk moveDirection on}]
                    },Cmd.none)
                else if on && model.player.anim ==Charge then
                    ({model|
                        player=model.player |> jumpdirection moveDirection
                    },Cmd.none)
                else if not on && model.player.anim /= Jump && model.player.speed.y == 0 then
                    ({model|
                        player=model.player |> stand
                        , record = model.record ++ [Debug.log "msg" { time = model.time, msg = AIWalk moveDirection on}]
                    },Cmd.none)
                else 
                    (model,Cmd.none)

        AnimCharge on->
                if on && model.player.anim == Stand then
                    ({model|
                        player=model.player |> charge 
                        , record = model.record ++ [Debug.log "msg" { time = model.time, msg = AICharge model.player.jumpdir on}]}, Cmd.none )
                else if not on && model.player.anim == Charge then
                    ({model| 
                        player=model.player |> jump
                        , record = model.record ++ [Debug.log "msg" { time = model.time, msg = AICharge model.player.jumpdir on}] }, Cmd.none)
                else    
                    (model,Cmd.none)

        Tick time -> 
            ( model |> animate time, Cmd.none )

        AnimAttack on->
            ( {model |
                player=model.player |> AnimState.attack}, Cmd.none )

        DebugRight on ->
            ( {model |
                        player=model.player |> changeDebugSpeed (Vector 1 0) on
                    }, Cmd.none )
        DebugLeft on ->
            ( {model |
                        player=model.player |> changeDebugSpeed (Vector -1 0) on
                    }, Cmd.none )
        DebugUp on ->
            ( {model |
                        player=model.player |> changeDebugSpeed (Vector 0 -1) on
                    }, Cmd.none )
        DebugDown on ->
                ( {model |
                        player=model.player |> changeDebugSpeed (Vector 0 1) on
                    }, Cmd.none )
        ExitDebugMode ->
            ( {model |
                    player=model.player |> stand
                    }, Cmd.none )
        Message.Start ->
            ( {model |
                    state = CG1_1, cgtime = 500 
                    }, Cmd.none)
        Next ->
            case model.state of 
                End ->
                    Model.init ()
                _ ->
                    ( model, Cmd.none )
        Back ->
            ( {model |
                    state = Model.Start
                    }, Cmd.none  )
        Message.About ->
            ( {model |
                    state = Model.About
                    }, Cmd.none)

        Jump1 ->
            ( {model |
                    player = initPlayer1, map = initMap1, state = One}, Cmd.none)
        JumpDiscoverI ->
            ( {model |
                    player = initPlayerDiscoverI initPlayer1, map = initMapDiscoverI, state = DiscoverI}, Cmd.none)
        Jump2 ->
            ( {model |
                    player = initPlayer2 initPlayer1, map = initMap2, state = Two, speedAI = initSpeedAI, time = 0}, Cmd.none)

        JumpDiscoverII ->
            ( {model |
                    player = initPlayerDiscoverII initPlayer1, map = initMapDiscoverII, state = DiscoverII}, Cmd.none)

        Jump3 ->
            ( {model |
                    player = initPlayer3 initPlayer1, map = initMap3, state = Three}, Cmd.none)

        _ ->
            ( model, Cmd.none )

changeDebugSpeed speed on player=
    if on then
        player|> setDebugSpeed speed
    else
        player|> setDebugSpeed (Vector 0 0)

setDebugSpeed speed player=
    { player| anim = DebugMode, speed = speed }
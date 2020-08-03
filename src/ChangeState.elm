module ChangeState exposing (..)
import Model exposing (..)

arriveExit model player =
    player.pos.x2 >= model.map.exit.x1 && player.pos.x1 <= model.map.exit.x2
 && player.pos.y1 <= model.map.exit.y2 && player.pos.y2 >= model.map.exit.y1

changeState model =
    let
        playerDiscoverI = initPlayerDiscoverI model.player
        player2 = initPlayer2 model.player
        playerDiscoverIIWin = initPlayerDiscoverII model.player
        playerDiscoverIILose = { playerDiscoverIIWin | inrage = True }
        player3 = initPlayer3 model.player
    in

    if arriveExit model model.player then 
        case model.state of
            One -> 
                { model | map = initMapDiscoverI, state = Story2_1, player = playerDiscoverI, time = 0}
            DiscoverI ->
                { model | map = initMap2, state = Story3_1, player = player2, time = 0}
            Two ->
                if arriveExit model model.speedAI then
                    { model | map = initMapDiscoverII, state = Story4_1, player = playerDiscoverIILose, time = 0}
                else
                    { model | map = initMapDiscoverII, state = Story4_1, player = playerDiscoverIIWin, time = 0}
            DiscoverII ->
                { model | map = initMap3, state = Story5_1, player = player3, time = 0}
            Three ->
                { model | map = initMap1, state = Story6_1, player = initPlayer1, time = 0}
            _ ->
                model
    else
        model

changeCGandStory time model =
    let 
        cgtime = model.cgtime - time
    in
    if model.cgtime >= 0 then
        { model | cgtime = cgtime}
    else 
        case model.state of
        -- LOGO
            LOGO ->
                if model.player.teachtextstate == 0 then
                    { model | state = Story1_1, cgtime = 500}
                else
                    { model | state = End, cgtime = 500}
        -- One
            Story1_1 ->
                { model | state = CG1_1, cgtime = 500 }
            CG1_1 ->
                { model | state = Story1_2, cgtime = 500 }
            Story1_2 ->
                { model | state = CG1_2, cgtime = 500 }
            CG1_2 ->
                { model | state = Story1_3, cgtime = 500 }
            Story1_3 -> 
                { model | state = CG1_3, cgtime = 500 }
            CG1_3 ->
                { model | state = Story1_4, cgtime = 500 }
            Story1_4 ->
                { model | state = CG1_4, cgtime = 500 }
            CG1_4 ->
                { model | state = One, cgtime = 500 }
        --DiscoverI
            Story2_1 ->
                { model | state = CG2_1, cgtime = 500 }        
            CG2_1 ->
                { model | state = Story2_2, cgtime = 500 }
            Story2_2 ->
                { model | state = CG2_2, cgtime = 500 }
            CG2_2 ->
                { model | state = DiscoverI, cgtime = 500 }
        --Two    
            Story3_1 ->
                { model | state = CG3_1, cgtime = 500 }
            CG3_1 ->
                { model | state = Two, cgtime = 500, speedAI = initSpeedAI, time = 0 }
        --DiscoverII        
            Story4_1 ->
                { model | state = DiscoverII, cgtime = 500 }
        --Three
            Story5_1 ->
                { model | state = CG5_1, cgtime = 500 }
            CG5_1 ->
                { model | state = Story5_2, cgtime = 500 }
            Story5_2 ->
                { model | state = CG5_2, cgtime = 500 }
            CG5_2 ->
                { model | state = Three, cgtime = 500 }
        --Ending
            Story6_1 ->
                { model | state = CG6_1, cgtime = 500 }
            CG6_1 ->
                { model | state = CG6_2, cgtime = 500 }
            CG6_2 ->
                { model | state = LOGO, cgtime = 500 }
            _ ->
                model
module Text exposing (..)
import Model exposing (..)
import Message exposing (..)

changeText state speedAI player=
    case state of
        One ->
            if  player.teachtextstate /= -1 then
                if player.pos.x1 >=0 && player.pos.x2 <= 1500 then
                    if player.anim == Stand && player.teachtextstate == 0 then
                        { player | text = "Use A and D to move right and left.", textframe = -500 }
                    else if player.anim == Walk && player.teachtextstate == 0 then
                        { player | teachtextstate = 1}
                    else if player.anim == Stand && player.teachtextstate == 1 then
                        { player | text = "Press Space or B to Jump.", textframe = -500 }
                    else if player.anim == Jump && player.teachtextstate == 1 then
                        { player | teachtextstate = 2 }
                    else if player.anim == Stand && player.teachtextstate == 2 then
                        { player | text = "Try to hold it longer...", textframe = -500 }     
                    else if player.chargetime >= 1500 && player.teachtextstate == 2 then
                        { player | teachtextstate = 3}
                    else if player.anim == Stand && player.teachtextstate == 3 then
                        { player | text = "Now, hold A or D while jumping.", textframe = -500, teachtextstate =4}
                    else if player.anim == Jump && player.speed.x == 0 && player.teachtextstate == 4 then
                        { player | teachtextstate = 5}
                    else if player.anim == Stand && player.teachtextstate == 5 then
                        { player | text = "You need to hold it until you release the space key!", textframe = -500}
                    else if  player.anim == Jump && player.speed.x /= 0 && player.pos.y2 >= 3090 && player.speed.y >0 && (player.teachtextstate == 5 || player.teachtextstate == 4) then
                        { player | teachtextstate = 6 }
                    else if player.teachtextstate == 6 then
                        { player | text = "You are ready to run away from here... Go right.", textframe = -500, teachtextstate = 7} 
                    else
                        { player | text = player.text }   
                else if player.pos.x1 <= 2650 && player.pos.x1 >= 2000 && player.teachtextstate == 7 then
                    { player | text = "See the soliers? Try to attack them using J.", textframe = -500, teachtextstate = 8}
                else if player.pos.x1 >= 1500 && player.teachtextstate < 7 then
                    { player | text = "You don't needs more instructions !?? Fine, good luck.....", textframe = -500, teachtextstate = -1} 
                else if player.anim == Attack && player.teachtextstate == 8 then
                    { player | teachtextstate = 9}
                else if player.teachtextstate == 9 then
                    { player | text = "You have learned everything you need. Now, just go up, up and up...", textframe = -200, teachtextstate = -1}
                else
                    { player | text = player.text}
            else if player.pos.y2 <= 970 && player.pos.x2 <= 3200 && player.anim == Stand then
                { player | text = "Finally", textframe = 0 }
            else if player.pos.y2 <= 970 && player.pos.x1 >= 3200 && player.anim == Stand then
                { player | text = "Cliff... ", textframe = -500 }
            else
                { player | text = player.text }

        DiscoverI ->
            if player.pos.x1 >= 1600 && player.pos.y1 >= 900 then
                { player | text = "NO!!!", textframe = 0 }
            else
                { player | text = player.text }

        Two ->
            if player.pos.y1 > (speedAI.pos.y1 + 500) then      --The latter "player" should be the AI in this level.
                { player | text = "I need to catch up!", textframe = 0 }
            else if player.pos.y2 <= 300 then
                { player | text = "No one can control me!", textframe = 0 }
            else if speedAI.pos.y2 <= 300 then     --The "player" should refer to the AI in this level.
                { player | text = "No! Please! I don't want to...", textframe = 0 }
            else
                { player | text = player.text }

        DiscoverII ->
            if player.pos.y2 <= 500 && player.anim == Stand then
                { player | text = "It's been 10 years... Now I'm back.", textframe = 0 }
            else
                { player | text = player.text }

        Three ->
            if player.pos.y2 >= 1800 && player.pos.y2 <= 2900 then
                { player | text = "I’m coming for you, Master Luke.", textframe = 0 }
            else if player.pos.y2 <= 350 then
                { player | text = "You are doomed…", textframe = 0 }
            else
                { player | text = player.text }  
        _ ->
            { player | text = player.text }

changeNPCText model npc = 
    case model.state of
        One->
            case npc.count of
                1 ->
                    { npc | text = "Nice to see you... Again?", textframe = -300}
                2 ->
                    { npc | text = "Nice to see you again... Again...", textframe = -300}
                3 ->
                    { npc | text = "Hey, do you really want to escape??", textframe = -300}
                4 ->
                    { npc | text = "Why don't you just stay here?", textframe = -300}
                _ ->
                    { npc | text = npc.text}
        DiscoverI->
            case npc.count of
                1 ->
                    { npc | text = "Hey!!! Control your power carefully!!!", textframe = -300}
                2 ->
                    { npc | text = "Fine, if you don't be careful... Hahaha....", textframe = -300} 
                3 ->
                    { npc | text = "OMG, is that XUAN????????", textframe = -300}
                _ ->
                    { npc | text = npc.text}
        Two->
            case npc.count of
                1 ->
                    { npc | text = "You have no chance to win XUAN anymore....", textframe = -300}
                _ -> 
                    { npc | text = npc.text}
        DiscoverII->
            case npc.count of
                1 ->
                    { npc | text = "I'm glad to see you!", textframe = -300}
                2 ->
                    { npc | text = "I'm glad to see you, AGAIN!", textframe = -300}
                3 ->
                    { npc | text = "Come on, again? AGAIN!??", textframe = -300}
                _ -> 
                    { npc | text = npc.text}
        Three->
            case npc.count of
                1->
                    { npc | text = "Don't you want to revenge?", textframe = -300}
                2 ->
                    { npc | text = "I can't understand you, why you even being here?", textframe = -300}
                _ -> 
                    { npc | text = npc.text}
        _->
            { npc | text = npc.text}

cleartext player =
    if player.textframe >= 100 then
        { player | text = "", textframe = 0}
    else
        { player | text = player.text }


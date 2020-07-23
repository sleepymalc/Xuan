module Text exposing (..)
import Model exposing (..)
import Message exposing (..)

changeText state player =
    case state of
        One ->
            if player.pos.y2 <= 3680 && player.pos.y2 >= 3500
            && player.pos.x1 <= 2650 && player.pos.x1 >= 2350 then
                { player | text = "I don’t think I can fight him…… ", textframe = 0 }
            else if player.pos.y2 <= 970 then
                { player | text = "Finally...", textframe = 0 }
            else
                { player | text = player.text }

        DiscoverI ->
            if player.pos.x1 >= 1600 && player.pos.y1 >= 900 then
                { player | text = "NO!!!", textframe = 0 }
            else
                { player | text = player.text }

        Two ->
            if player.pos.y1 < player.pos.y1 then      --The latter "player" should be the AI in this level.
                { player | text = "I need to catch up!", textframe = 0 }
            else if player.pos.y2 <= 300 then
                { player | text = "No one can control me!", textframe = 0 }
            else if player.pos.y2 <= 300 then     --The "player" should refer to the AI in this level.
                { player | text = "No! Please! I don't want to...", textframe = 0 }
            else
                { player | text = player.text }

        DiscoverII ->
            if player.pos.y2 <= 500 then
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

        --_ ->
          --  { player | text = player.text }


cleartext player =
    if player.textframe >= 100 then
        { player | text = "", textframe = 0}
    else
        { player | text = player.text }
module Text exposing (..)
import Model exposing (..)
import Message exposing (..)

changeText state player =
    case state of
        One ->
            if player.pos.y1 <= 3680 && player.pos.y1 >= 3500
            && player.pos.x1 <= 2650 && player.pos.x1 >= 2350 then
                { player | text = "I don’t think I can fight him…… ", textframe = 0 }
            else if player.pos.y1 <= 970 then
                { player | text = "Finally...", textframe = 0 }
            else
                { player | text = player.text }

        _ ->
            { player | text = player.text }


cleartext player =
    if player.textframe >= 100 then
        { player | text = "", textframe = 0}
    else
        { player | text = player.text }
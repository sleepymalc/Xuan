module Text exposing (..)
import Model exposing (..)
import Message exposing (..)

changeText state speedAI player=
    case state of
        One ->
            if player.pos.x1 >=0 && player.pos.x2 <= 1500 && player.teachtextstate /= -1 then
                if player.anim == Stand && player.teachtextstate == 0 then
                { player | text = "Use A and D to move right and left.", textframe = -500 }
                else if player.anim == Walk && player.teachtextstate == 0 then
                { player | teachtextstate = 1}
                else if player.anim == Stand && player.teachtextstate == 1 then
                { player | text = "Press Space to Jump.", textframe = -500 }
                else if player.anim == Jump && player.teachtextstate == 1 then
                { player | teachtextstate = 2 }
                else if player.anim == Stand && player.teachtextstate == 2 then
                { player | text = "Try to hold it longer...", textframe = -500 }     
                else if player.chargetime >= 1500 && player.teachtextstate == 2 then
                { player | teachtextstate = 3}
                else if player.anim == Stand && player.teachtextstate == 3 then
                { player | text = "Now, before releasing Space key, hold A or D.", textframe = -500 }
                else if  player.anim == Jump && player.speed.x /= 0 && player.pos.y2 >= 3090 &&  player.teachtextstate == 3 then
                { player | teachtextstate = 4 }
                else if player.teachtextstate == 4 then
                { player | text = "You are ready to run away from here... Be careful...", textframe = -500, teachtextstate = -1} 
                else
                { player | text = player.text }   
            else if player.pos.y2 <= 3680 && player.pos.y2 >= 3500
            && player.pos.x1 <= 2650 && player.pos.x1 >= 2350 then
                { player | text = "I don’t think I can fight him…… ", textframe = 0 }
            else if player.pos.y2 <= 970 && player.anim == Stand then
                { player | text = "Finally...", textframe = 0 }
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
        _ ->
            { player | text = player.text }


cleartext player =
    if player.textframe >= 100 then
        { player | text = "", textframe = 0}
    else
        { player | text = player.text }

changeStory state story =
    case state of
            StoryOne ->
                { story | text =  "Old man：\"Poor kid, to improve the status of your family, your father and I decide to send you to attend the experiments in our sect. For the rest of your life, you will need to practice all kinds"
                                 ++"of internal skills and receive all kinds of stimulations. Don’t feel unfair. This is the brutality of the sects. Without sacrifice, there will be no strong sects.\""
                                 ++"A mysterious voice: \"Bullshit! How can anyone believe this old bastard? Sending your apprentice to be a testimony and feel sorry for him? You are one of a kind!\""
                                 ++"Song：\"Who are you? What’s that voice in my head? What’s going on with me?\""
                                 ++"A mysterious voice:\"You’re wondering who I am? My name is XUAN, and I can set you free.\""
                , storyframe = 0
                }

            StoryTwo ->
                { story | text = ""
                , storyframe = 0
                }

            StoryThree ->
                { story | text = ""
                , storyframe = 0
                }

            StoryFour ->
                { story | text = ""
                , storyframe = 0
                }

            StoryFive ->
                { story | text = ""
                , storyframe = 0
                }

            StorySix ->
                { story | text = ""
                , storyframe = 0
                }

            _ ->
                { story | text = story.text }








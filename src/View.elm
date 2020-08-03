module View exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style,src,controls,autoplay,loop,attribute)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Model exposing (..)
import Message exposing (..)
import Update exposing (..)
import Animate exposing (..)
import MapSetting exposing (..)
import Load exposing (..)

viewAttrs =
    { size = Vector 1600 800
    }

textStoryOne = "story one"

view : Model -> Html Msg
view model =
    let
        renderSvg = svg
                        (gameUIAttribute model.size)
                        (if model.state == Story1_1 || model.state == Story1_2 || 
                            model.state == Story1_3 || model.state == Story1_4 ||
                            model.state == Story2_1 || model.state == Story2_2 ||
                            model.state == Story3_1 || model.state == Story4_1 ||
                            model.state == Story5_1 || model.state == Story5_2 || 
                            model.state == Story6_1 then
                            [renderStory model.story]
                        else if model.state == CG1_1 || model.state == CG1_2 ||
                                model.state == CG1_3 || model.state == CG1_4 ||
                                model.state == CG2_1 || model.state == CG2_2 ||
                                model.state == CG3_1 || model.state == CG5_1 ||
                                model.state == CG5_2 || model.state == CG6_1 ||
                                model.state == CG6_2 then 
                            [renderCG model]
                        else if model.state == Loading then
                            [renderS 20 100 1 "Loading..."]
                        else if model.state == LOGO then
                            [renderCG model, renderBackground model]
                        else
                        ([ renderBackground model
                        , renderPlayer model.state model.player
                        ]
                        ++ renderCharacters model.player model.map.characters
                        
                        --++ debugCollision model.map.characters model.player
                        --++ debugAttack model.map.characters model.player
                        ++ renderNPCs model.player model.map.npcs 
                        ++ ( if model.state == Two then
                                renderSpeedAI model.player model.speedAI
                           else
                                []
                        )
                        ++ [renderPlayerText model.player]
                        ++ renderNPCsText model.player model.map.npcs
                        ++ renderBlood model.player
                        ))                    
        renderLoad = if model.state == Loading then
                        List.map loadImg initLoadPack
                    else []
        renderHtml = renderbricks (List.map .pos model.map.bricks) model

    in  div[]
        [         
            div
                [ Html.Attributes.style "height" "0px"]
                [ renderSvg
                , span[Html.Attributes.style "opacity" "0"] renderLoad
                ]    
        , div []renderHtml

        ]

renderSpeedAI player speedAI= 
    let
        url = getAnimUrl speedAI.anim speedAI.frame speedAI ""
        attr = getDirectionAttr speedAI.direction
        viewpos = speedAI.pos |> offset player |> resizePlayer|> clearOutsideImage
    in
        [renderImage url viewpos attr]

renderBlood player = 
    if player.mood == Normal then
        if player.hp >= 7 && player.hp <10 then
            [ renderImage "img/Effect/bloodFrame_1.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeOne)/2000))]
            ]
        else if player.hp >= 5 && player.hp < 7 then
            [ renderImage "img/Effect/bloodFrame_1.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeTwo)/1750))]
            , renderImage "img/Effect/bloodFrame_2.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeOne)/1750))]           
            ]
        else if player.hp >= 3 && player.hp < 5 then
            [ renderImage "img/Effect/bloodFrame_1.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeThree)/1500))]
            , renderImage "img/Effect/bloodFrame_2.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeTwo)/1500))] 
            , renderImage "img/Effect/bloodFrame_3.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeThree)/1500))]
            ] 
        else if player.hp > 0 && player.hp < 3 then
            [ renderImage "img/Effect/bloodFrame_1.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeFour)/1250))]
            , renderImage "img/Effect/bloodFrame_2.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeTwo)/1250))] 
            , renderImage "img/Effect/bloodFrame_3.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeThree)/1250))]
            , renderImage "img/Effect/bloodFrame_4.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeFour)/1250))]
            ]  
        else
            [ renderImage "img/Effect/blood_1.png" (Pos 0 0 0 0) [opacity "1"]
            ]
    else 
        [ renderImage "img/Effect/bloodFrame_1.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeFive)/1000))]
        , renderImage "img/Effect/bloodFrame_2.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeThree)/1000))] 
        , renderImage "img/Effect/bloodFrame_3.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeFour)/1000))]
        , renderImage "img/Effect/bloodFrame_4.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeFive)/1000))]
        , renderImage "img/Effect/bloodFrame_5.png" (Pos 0 1600 0 800) [opacity (String.fromFloat (abs(player.effecttimeFive)/1000))]
        ]

        

offset player pos =
    let
        dx = toFloat(floor (player.pos.x1/viewAttrs.size.x) * viewAttrs.size.x)
        dy = toFloat(floor (player.pos.y1/viewAttrs.size.y)* viewAttrs.size.y)
    in
        Pos (pos.x1-dx) (pos.x2-dx) (pos.y1-dy) (pos.y2-dy)

renderbricks posList model=
    List.map (renderbrick1 model) posList

renderbrick1 model pos=
    let
        viewpos = pos |> offset model.player
                    |> cutBrickView
                    |> clearOutsideImage
        text = if model.state == One then
                    "img/map_1/stone_1.png"
                else if model.state == DiscoverI then
                    "img/map_2/stone_1.png"
                else if model.state == Two then
                    "img/map_3/stone_1.png"
                else if model.state == DiscoverII then
                    "img/map_4/stone_1.png"
                else if model.state == Three then
                    "img/map_5/stone_1.png"
                else
                    "img/map_1/stone_1.png"
    in
        renderHtmlImg model.size text viewpos
        --renderImage text viewpos []

    

    
renderbrick player pos=
    let
        viewpos = pos |> offset player
                    |> cutBrickView
                    |> clearOutsideImage
        
    in
    rect
        [ x (String.fromFloat viewpos.x1)
        , y (String.fromFloat viewpos.y1)
        , width (String.fromFloat (viewpos.x2-viewpos.x1))
        , height (String.fromFloat (viewpos.y2-viewpos.y1))
        , fill "#000000"
        ]
        []

cutBrickView viewpos =
    let
        x1 = if viewpos.x1<0 then
                    0
                else
                    viewpos.x1
        x2 = if viewpos.x2>viewAttrs.size.x then
                    viewAttrs.size.x
                else
                    viewpos.x2
        y1 = if viewpos.y1<0 then
                    0
                else
                    viewpos.y1
        y2 = if viewpos.y2>viewAttrs.size.y then
                    viewAttrs.size.y
                else
                    viewpos.y2
    in
        Pos x1 x2 y1 y2



renderBackground model=
    if model.state == LOGO then
       renderImage "img/background.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [opacity (String.fromFloat (1-(model.cgtime/1000-2.5)^4/40))] 
    else if model.state == One then
        if model.player.pos.y1 >= 3200 || (model.player.pos.y1 <= 1600 && model.player.pos.x1 >= 3200 )then
            renderImage "img/background/background1_2.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) []
        else 
            renderImage "img/background/background1_1.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) []
    else if model.state == DiscoverI then
        renderImage "img/background/background2_1.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [] 
    else if model.state == Two then
        renderImage "img/background/background3_1.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [] 
    else if model.state == DiscoverII then
        renderImage "img/background/background4_1.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [] 
    else if model.state == Three then
        renderImage "img/background/background5_1.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [] 
    else
       renderImage "img/background.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [opacity "0"] 

renderCharacters player characters=
    List.map (renderCharacter player)characters 

renderCharacter player character =
    let
        url = getAnimUrl character.anim character.frame character "character_"
        attr = getDirectionAttr character.direction
        viewpos = character.pos |> offset player |> resizePlayer|> clearOutsideImage
        --character.pos
        --getPlayerViewPos character

    in
        renderImage url viewpos attr

clearOutsideImage viewpos=
    if viewpos.x2<0 || viewpos.x1>viewAttrs.size.x || viewpos.y2<0 || viewpos.y1>viewAttrs.size.y then
        Pos -1600 -1600 0 0
    else
        viewpos

connectName namePrefix anim id=
    namePrefix ++ anim ++ "/" ++ namePrefix ++ anim ++"_"
    ++ String.padLeft 4 '0' (String.fromInt id)

--Todo: add whole rage picture(blood or something else)
getAnimUrl anim frame player namePrefix= 
    let
        --prefix = "http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/character/color/"
        prefix = "img/character/color/"
        surfix = ".png"
        name = case anim of
            Stand -> 
                    connectName namePrefix "walk" 0
            Walk ->
                let
                    id = if namePrefix == "" then
                            modBy 65 frame
                        else 
                            modBy 60 frame

                in
                    connectName namePrefix "walk" id
            Charge ->
                let
                    id = if frame < 50 then 
                                1 
                            else if frame < 120 then 
                                0 
                            else 
                                2
                in
                    connectName namePrefix "charge" id
                -- might use chargetime to decide       
            Jump -> 
                    connectName namePrefix "jump" 0
            Attack ->
                let
                    id = modBy 60 frame
                in
                    connectName namePrefix "attack" id

            Crouch ->
                    connectName namePrefix "charge" 2

            Grovel ->
                    connectName namePrefix "grovel" 0

            Attacked ->
                if ( player.speed.x < 0 && player.direction == Left)
                || ( player.speed.x > 0 && player.direction == Right) then
                    namePrefix++"attacked/"++namePrefix++"attackedBack_0000"
                else
                    namePrefix++"attacked/"++namePrefix++"attackedFront_0000"
            Fly ->
                    connectName namePrefix "jump" 0
            DebugMode ->
                    connectName namePrefix "walk" 0
    in
        prefix ++ name ++ surfix



getDirectionAttr direction =
    case direction of
           Left ->
            [ transform "scale (-1 1)"]
           Right ->
            []

getPlayerViewPos player =
    player.pos 
    |> offset player 
    |> resizePlayer

renderPlayer state player=
    let
        url = getAnimUrl player.anim player.frame player ""
        attr = if state == Two then
                    getDirectionAttr player.direction ++ [opacity "0.5"]
               else
                    getDirectionAttr player.direction
        pos = getPlayerViewPos player
    in
        if player.anim == Grovel then
            renderImage url (Pos (pos.x1+85) (pos.x2-85) (pos.y1+30) (pos.y2+30)) attr
        else
            renderImage url pos attr


renderNPC player npc=
    let
        url = getAnimUrl npc.anim npc.frame npc "NPC_"
        attr = getDirectionAttr npc.direction
        pos = npc.pos |> offset player |> clearOutsideImage

    in    
    renderImage url (Pos pos.x1 pos.x2 (pos.y1+10) (pos.y2+10)) attr

renderNPCs player npcs =
    List.map (renderNPC player) npcs

debugCollision characters player=
    let
        charactersCollisionPos = characters
            |> List.map .collisionPos
            |> List.concat
        collisionPos = player.collisionPos ++ charactersCollisionPos  
            |> List.map (offset player) 
            |> List.map clearOutsideImage

    in
        List.map (\pos ->
                    rect
                        [ x (String.fromFloat pos.x1)
                        , y (String.fromFloat pos.y1)
                        , width (String.fromFloat (pos.x2-pos.x1))
                        , height (String.fromFloat (pos.y2-pos.y1))
                        , opacity "0.2"
                        , fill "#000000"
                        ]
                        []) collisionPos 

debugAttack characters player=
    let
        attackPos = ((playerAttackRange player)
            :: (characters 
            |> List.map attackRange))
            |> List.map (offset player) 
            |> List.map clearOutsideImage
    in
        List.map (\pos ->
                    rect
                        [ x (String.fromFloat pos.x1)
                        , y (String.fromFloat pos.y1)
                        , width (String.fromFloat (pos.x2-pos.x1))
                        , height (String.fromFloat (pos.y2-pos.y1))
                        , opacity "0.2"
                        , fill "#ff0000"
                        ]
                        []) attackPos 

resizePlayer pos =
    let
        y = 768 / 700 * (pos.y2-pos.y1)
        y2 = pos.y2
        y1 = y2 - y
        x = 1366 / 455 * (pos.x2-pos.x1)
        dx = (x - (pos.x2-pos.x1))/2
        x1 = pos.x1 - dx
        x2 = pos.x2 + dx
    in
-- 455 -> 130; 700 ->200
-- 1366 768
    Pos x1 x2 y1 y2



gameUIAttribute size= 
    [ width (String.fromFloat size.x)
    , height (String.fromFloat size.y)
    , viewBox ("0 0 "++(String.fromFloat viewAttrs.size.x) ++ " " ++ (String.fromFloat viewAttrs.size.y)) 
    ]




renderAudio url =
    audio
        [src url, autoplay True]
        [Html.text "Your browser does not support the audio"]


   
renderButton msg url size pos= 
    button
        [  Html.Attributes.style "border" "0"
        , Html.Attributes.style "bottom" "30px"
        , Html.Attributes.style "cursor" "pointer"
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "height" "0px"
        , Html.Attributes.style "left" "0px"
        , Html.Attributes.style "line-height" "60px"
        , Html.Attributes.style "outline" "none"
        , Html.Attributes.style "padding" "0"
        -- Display at center
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" ((String.fromFloat pos.x)++"px")
        , Html.Attributes.style "top" ((String.fromFloat pos.y)++"px")
        -- 
        , Html.Attributes.style "width" "120px"
        , onClick msg
        ]
        [ Html.img [src url
        , height ((String.fromFloat size.y)++"px") 
        , width ((String.fromFloat size.x)++"px")][] ]

renderHtmlImg size url svgpos=
    let
        pos = svgpos|>toHtmlPos size
    in
        Html.img [src url
            , height ((String.fromFloat (pos.y2-pos.y1))++"px") 
            , width ((String.fromFloat (pos.x2-pos.x1))++"px")
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" ((String.fromFloat pos.x1)++"px")
            , Html.Attributes.style "top" ((String.fromFloat pos.y1)++"px")][]
            
toHtmlPos size pos =
    let
        dy = size.y/2 - size.x/2/2
        x1 = pos.x1 /1600 * size.x
        x2 = pos.x2 /1600 * size.x
        y1 = pos.y1 /1600 * size.x + dy
        y2 = pos.y2 /1600 * size.x+dy
    in
        Pos x1 x2 y1 y2



renderImage url pos attr=
    Svg.image
        ([  xlinkHref url
        , width (String.fromFloat (pos.x2-pos.x1))
        , height (String.fromFloat (pos.y2-pos.y1))
        , if List.member (transform "scale (-1 1)") attr then
             x (String.fromFloat (-pos.x2))
            else
            x (String.fromFloat pos.x1)
        , y (String.fromFloat pos.y1)
        ]
        ++ attr)
    []

renderText size xx yy text =
    Svg.text_
        [ fontSize (String.fromFloat size)
        ]
        [ tspan [x (String.fromFloat xx),
            y (String.fromFloat yy)]
            [Svg.text text]
        ]

renderT size pos w lines text =
    foreignObject [ x (String.fromFloat (pos.x1+70))
                  , y (String.fromFloat (pos.y1-(toFloat(lines)*20)))
                  , width (String.fromFloat w)
                  , height (String.fromFloat (toFloat(lines)*22))
                  ]
                  [ p [ Html.Attributes.style "font-size" ((String.fromFloat size)++"px") ]
                      [ Svg.text text ]
                  ]

renderTyper size pos w lines text frame =
    text
        |> String.left (floor ((toFloat frame)/10))
        |> renderT size pos w lines



renderPlayerText player =
    let
        size = 18
        pos = getPlayerViewPos player
        w = 180
        lines = (floor(toFloat(String.length(player.text))/20)+3)
        text = player.text
    in
        renderT size pos w lines text

renderNPCsText player npcs = 
    List.map (renderNPCText player) npcs

renderNPCText player npc = 
    let
        size = 18
        pos = (Pos (npc.pos.x1-150) (npc.pos.x2-150) (npc.pos.y1+10) (npc.pos.y2+10)) |> offset player |> clearOutsideImage
        w = 180
        lines = (floor(toFloat(String.length(npc.text))/20)+2) 
    in
        renderT size pos w lines npc.text

renderS size w lines text =
    foreignObject [ x "200"
                  , y "80"
                  , width (String.fromFloat w)
                  , height (String.fromFloat (toFloat(lines)*20))
                  ]
                  [ p [ Html.Attributes.style "font-size" ((String.fromFloat size)++"px") ]
                      [ Svg.text text ]
                  ]

renderStory story =
    let
        size = 20
        w = 1000
        lines = (floor(toFloat(String.length(story.text))/20)+2)
        text = story.text
    in
        renderS size w lines text

renderCG model = 
    case model.state of 
        CG1_1 ->
            renderImage "img/CG/CG1/CG1_1.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [opacity (String.fromFloat (1-(model.cgtime/1000-2.5)^4/40))]
        CG1_2 ->
            renderImage "img/CG/CG1/CG1_2.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [opacity (String.fromFloat (1-(model.cgtime/1000-2.5)^4/40))]
        CG1_3 ->
            renderImage "img/CG/CG1/CG1_3.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [opacity (String.fromFloat (1-(model.cgtime/1000-2.5)^4/40))]
        CG1_4 ->
            renderImage "img/CG/CG1/CG1_4.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [opacity (String.fromFloat (1-(model.cgtime/1000-2.5)^4/40))]
        LOGO ->
            renderImage "img/LOGO.png" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) [opacity (String.fromFloat (1-(model.cgtime/1000-2.5)^4/40))]
        _ ->
            renderImage "img/background.png" (Pos 0 0 0 0) []


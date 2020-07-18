module View exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style,src,controls,autoplay,loop,attribute)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)

import Model exposing (..)
import Message exposing (..)


view : Model -> Html Msg
view model =
    let
        renderSvg =[ svg
                        (gameUIAttribute model.size)
                        ([ renderBackground 
                        , renderPlayer model.player]
                        ++ renderCharacters model.player model.map.characters
                        ++ debugCollision model.player
                        ++ renderbricks (List.map .pos model.map.bricks) model.player
                        )
                    ]
        renderHtml = []
    in        
        div
            []
            [ span[]renderSvg
            , span[]renderHtml
            ]

offset player pos =
    let
        dx = toFloat(floor (player.pos.x1/1600) * 1600)
        dy = toFloat(floor (player.pos.y1/800)* 800)
    in
        Pos (pos.x1-dx) (pos.x2-dx) (pos.y1-dy) (pos.y2-dy)

renderbricks posList player=
    List.map (renderbrick player) posList

renderbrick player pos=
    let
        viewpos = pos |> offset player
        x1 = if viewpos.x1<0 then
                    0
                else
                    viewpos.x1
        x2 = if viewpos.x2>1600 then
                    1600
                else
                    viewpos.x2
        y1 = if viewpos.y1<0 then
                    0
                else
                    viewpos.y1
        y2 = if viewpos.y2>800 then
                    800
                else
                    viewpos.y2
    in
    rect
        [ x (String.fromFloat x1)
        , y (String.fromFloat y1)
        , width (String.fromFloat (x2-x1))
        , height (String.fromFloat (y2-y1))
        , fill "#000000"
        ]
        []

renderBackground =
    renderImage "img/background.jpg" (Pos 0 1600 0 800) []

renderCharacters player characters=
    List.map (renderCharacter player)characters 

renderCharacter player character =
    let
        url = getAnimUrl character.anim character.frame
        attr = getDirectionAttr character.direction
        viewpos = character.pos |> offset player |> resizePlayer|> clearOutsideImage
        --character.pos
        --getPlayerViewPos character

    in
        renderImage url viewpos attr

clearOutsideImage viewpos=
    if viewpos.x2<0 || viewpos.x1>1600 || viewpos.y2<0 || viewpos.y1>800 then
        Pos 0 0 0 0
    else
        viewpos
getAnimUrl anim frame= 
    let
        prefix = "img/character/"
        surfix = ".png"
        name = case anim of
            Stand -> 
                "color/walk/walk_0000"
            Run ->
                "run/run_"
            Walk ->
                "color/walk/walk_" ++ String.padLeft 4 '0' (String.fromInt (modBy 65 frame))
            Charge ->
                "color/charge/charge_" ++ String.padLeft 4 '0' (String.fromInt
                    (if frame < 50 then 1 
                        else if frame < 120 then 0 else 2)) 
                -- might use chargetime to decide       
            Jump -> 
                "color/jump/jump_0000"

            Attack ->
                "color/attack/attack_" ++ (String.padLeft 4 '0' (String.fromInt (modBy 60 frame)))
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

renderPlayer player= 
    let
        url = getAnimUrl player.anim player.frame
        attr = getDirectionAttr player.direction
        pos = getPlayerViewPos player

    in
        renderImage url pos attr

debugCollision player=
    let
        collisionPos = List.map (offset player) player.collisionPos
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

resizePlayer pos =
    let
        y= 768 / 700 * (pos.y2-pos.y1)
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
    , viewBox "0 0 1600 800"
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

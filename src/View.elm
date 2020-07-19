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


viewAttrs =
    { size = Vector 1600 800
    }

view : Model -> Html Msg
view model =
    let
        renderSvg =[ svg
                        (gameUIAttribute model.size)
                        ([ renderBackground 
                        , renderPlayer model.player]
                        ++ renderCharacters model.player model.map.characters
                        --++ debugCollision model.map.characters model.player
                        --++ debugAttack model.map.characters model.player
                        ++ renderbricks (List.map .pos model.map.bricks) model.player
                        ++ [renderPlayerText model.player]
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
        dx = toFloat(floor (player.pos.x1/viewAttrs.size.x) * viewAttrs.size.x)
        dy = toFloat(floor (player.pos.y1/viewAttrs.size.y)* viewAttrs.size.y)
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
    rect
        [ x (String.fromFloat x1)
        , y (String.fromFloat y1)
        , width (String.fromFloat (x2-x1))
        , height (String.fromFloat (y2-y1))
        , fill "#000000"
        ]
        []

renderBackground =
    renderImage "img/background.jpg" (Pos 0 viewAttrs.size.x 0 viewAttrs.size.y) []

renderCharacters player characters=
    List.map (renderCharacter player)characters 

renderCharacter player character =
    let
        url = getAnimUrl character.anim character.frame character
        attr = getDirectionAttr character.direction
        viewpos = character.pos |> offset player |> resizePlayer|> clearOutsideImage
        --character.pos
        --getPlayerViewPos character

    in
        renderImage url viewpos attr

clearOutsideImage viewpos=
    if viewpos.x2<0 || viewpos.x1>viewAttrs.size.x || viewpos.y2<0 || viewpos.y1>viewAttrs.size.y then
        Pos 0 0 0 0
    else
        viewpos
getAnimUrl anim frame player= 
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

            Crouch ->
                "color/charge/charge_0002"

            Attacked ->
                if ( player.speed.x < 0 && player.direction == Left)
                || ( player.speed.x > 0 && player.direction == Right) then
                    "color/attacked/attackedBack_0000"
                else
                    "color/attacked/attackedFront_0000"
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
        url = getAnimUrl player.anim player.frame player
        attr = getDirectionAttr player.direction
        pos = getPlayerViewPos player
    in
        renderImage url pos attr


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

--renderText size pos text lines=
--    Svg.text_
--        [ fontSize (String.fromFloat size)
--        ]
--        [ tspan [x (String.fromFloat (pos.x1+70)),
--            y (String.fromFloat (pos.y1-(toFloat(lines)*20)))]
--            [Svg.text text]
--        ]

renderT size pos w lines text =
    foreignObject [ x (String.fromFloat (pos.x1+70))
                  , y (String.fromFloat (pos.y1-(toFloat(lines)*20)))
                  , width (String.fromFloat w)
                  , height (String.fromFloat (toFloat(lines)*20))
                  ]
                  [ p [ fontSize (String.fromFloat size) ]
                      [ Svg.text text ]
                  ]


renderPlayerText player=
    let
        size = 20
        pos = getPlayerViewPos player
        w = 180
        lines = (floor(toFloat(String.length(player.text))/20)+2)
        text = player.text
    in
        renderT size pos w lines text
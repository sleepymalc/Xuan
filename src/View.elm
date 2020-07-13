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
                        [renderPlayer model.player]
                    ]
        renderHtml = []
    in        
        div
            []
            [ span[]renderSvg
            , span[]renderHtml
            ]

renderPlayer player= 
    let
        prefix = "img/character/"
        surfix = ".png"
        name = case player.anim of
            Stand -> 
                "stand/stand_0000"
            Run ->
                "run/run_"
            Walk ->
                "walk/walk_00" ++ (String.fromInt (
                    if player.frame <= 33 then 
                        player.frame
                    else (modBy 33 player.frame) + 33))
            Jump -> 
                "jump/jump_000"
        attr = case player.direction of
           Left ->
            []
           Right ->
            [ transform "scale (-1 1)"]
    in
        renderImage (prefix ++ name ++ surfix) player.pos attr



gameUIAttribute size= 
    [ width (String.fromFloat size.x)
    , height (String.fromFloat size.y)
    , viewBox "0 0 1600 1000"
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
             x (String.fromFloat (-pos.x2+100))
            else
            x (String.fromFloat pos.x1)
        , y (String.fromFloat pos.y1)
        ]
        ++ attr)
    []

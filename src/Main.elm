module Main exposing (main)

import Html exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Model exposing (..)
import View exposing (..)
import Message exposing (..)
import Update exposing (..)
import AnimState exposing(..)
import MapSetting exposing (..)


main =
    Browser.element
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.player.mood == Rage then
    Sub.batch
        [ onAnimationFrameDelta Tick --the time in 1/1000s since the previous frame
        , onKeyUp (Decode.map (keyRage False) keyCode)
        , onKeyDown (Decode.map (keyRage True) keyCode)
        , onResize Resize
        ]
    else
    Sub.batch
        [ onAnimationFrameDelta Tick --the time in 1/1000s since the previous frame
        , onKeyUp (Decode.map (keyNormal False model) keyCode)
        , onKeyDown (Decode.map (keyNormal True model) keyCode)
        , onResize Resize
        ]

keyNormal : Bool -> Model -> Int -> Msg
keyNormal on model keycode =
    case keycode of 
        --A
        65 ->
            AnimWalk Left on
        --D
        68 ->
            AnimWalk Right on
        --Space
        32 ->
            AnimCharge on
        --B
        66 ->
            AnimCharge on
        --J
        74 ->
            AnimAttack on
        --Left

       -- 37 ->
       --     DebugLeft on
        --Up
        --38 ->
        --    DebugUp on
        --Right
        --39 ->
        --    DebugRight on
        -- Down
        --40 ->
        --    DebugDown on
        -- P
       -- 80 ->
         --   DebugPos
        --Enter
        --13 ->
        --    ExitDebugMode 
        --49 ->
        --    Jump1
        --50 ->
        --    JumpDiscoverI
        --51 ->
        --    Jump2
        --52 ->
        --    JumpDiscoverII
        --53 ->
        --    Jump3
        _ ->
            Noop 

keyRage : Bool -> Int -> Msg
keyRage on keycode =
    case keycode of 
        --A
        68 ->
            AnimWalk Left on
        --D
        65 ->
            AnimWalk Right on
        --Space
        32 ->
            AnimCharge on
        --B
        66 ->
            AnimCharge on
        --J
        74 ->
            AnimAttack on
 {-       
        --Left
        37 ->
            DebugLeft on
        --Up
        38 ->
            DebugUp on
        --Right
        39 ->
            DebugRight on
        -- Down
        40 ->
            DebugDown on
        --Enter
        13 ->
            ExitDebugMode 
-}

        _ ->
            Noop

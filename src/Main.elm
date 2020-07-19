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
    Sub.batch
        [ onAnimationFrameDelta Tick --the time in 1/1000s since the previous frame
        , onKeyUp (Decode.map (key False) keyCode)
        , onKeyDown (Decode.map (key True) keyCode)
        , onResize Resize
        ]

key : Bool -> Int -> Msg
key on keycode =
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
        --J
        74 ->
            AnimAttack on
        _ ->
            Noop

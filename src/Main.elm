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
        [ --if model.state == Playing then
            onAnimationFrameDelta Tick --the time in 1/1000s since the previous frame
          --else
            --Sub.none
        , onKeyUp (Decode.map (key model False) keyCode)
        , onKeyDown (Decode.map (key model True) keyCode)
        , onResize Resize
        ]



--key : Bool -> Int -> Msg
key model on keycode =
    case keycode of 
        65 ->
            AnimWalk Left on     

        68 ->
            AnimWalk Right on

        32 ->
            AnimCharge  on --

        _ ->
            Noop

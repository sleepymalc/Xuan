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
            onAnimationFrameDelta Tick
          --else
            --Sub.none
        , onKeyUp (Decode.map (key False) keyCode)
        , onKeyDown (Decode.map (key True) keyCode)
        , onResize Resize
        ]



--key : Bool -> Int -> Msg
key on keycode =
    case keycode of 
        65 ->
            AnimWalk Left on     

        68 ->
            AnimWalk Right on

        32 ->
            AnimJump on

        _ ->
            Noop
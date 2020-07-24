module Load exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Message exposing (..)

loadImg url =
    img
        [ src url
        , on "error" (Json.Decode.succeed (ImageError url))
        , on "load" (Json.Decode.succeed (ImageLoaded url))
        ]
        []

connectName namePrefix anim id=
    namePrefix ++ anim ++ "/" ++ namePrefix ++ anim ++"_"
    ++ String.padLeft 4 '0' (String.fromInt id)

initLoadPack =
    let
        prefix = "http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/character/color/"
        surfix = ".png"
        names = (List.range 0 64
            |> List.map (connectName "" "walk"))
            ++ (List.range 0 59
            |> List.map (connectName "character_" "walk"))
            ++ (List.range 0 59
            |> List.map (connectName "" "attack"))
            ++ (List.range 0 59
            |> List.map (connectName "character_" "attack"))
            ++ (List.range 0 2
            |> List.map (connectName "" "charge"))
            ++
            [ connectName "" "jump" 0
            , "attacked/attackedBack_0000"
            , "attacked/attackedFront_0000"]
        urls = List.map (\name-> prefix ++ name ++ surfix) names
    in
        urls
        

        
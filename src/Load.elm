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
        urls =  List.map (\name-> prefix ++ name ++ surfix) names
                ++ ["http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/background.png"]
                ++ ["http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/CG1_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/CG1_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/CG1_3.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/CG1_4.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/CG2_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/CG2_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/CG3_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/CG5_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/CG5_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/CG6_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story1_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story1_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story1_3.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story1_4.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story1_5.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story2_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story2_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story3_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story3_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story4_Lose.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story4_Win.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story5_0.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story5_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story5_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/CG/Story6_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/Page/LOGO.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/Page/About.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/Page/Start.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/Page/Break.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/Page/Exit.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/Effect/bloodFrame_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/Effect/bloodFrame_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/Effect/bloodFrame_3.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/Effect/bloodFrame_4.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/Effect/bloodFrame_5.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/map_1/stone_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/map_1/stone_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/map_2/stone_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/map_2/stone_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/map_3/stone_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/map_3/stone_2.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/map_3/stone_3.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/map_4/stone_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/map_5/stone_1.png"
                   ,"http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/map_5/stone_2.png"



                   
                   
                   
                   
                   
                   
                   
                    ]
    in
        urls
        

        
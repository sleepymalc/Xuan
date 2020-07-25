module Collision exposing (..)
import MapSetting exposing (..)


rightImpact speed time posList playerPos =
    (posList|> List.filter (projectionOverlap .y1 .y2 playerPos)
            |> List.filter (\pos -> (playerPos.x2 - speed.x * time < pos.x1) && (playerPos.x2 > pos.x1))
            |> List.isEmpty) == False

leftImpact speed time posList playerPos =
    (posList|> List.filter (projectionOverlap .y1 .y2 playerPos)
            |> List.filter (\pos -> (playerPos.x1 - speed.x * time > pos.x2) && (playerPos.x1 < pos.x2))
            |> List.isEmpty) == False

downImpact speed time posList playerPos =
    (posList|> List.filter (projectionOverlap .x1 .x2 playerPos)
            |> List.filter (\pos -> (playerPos.y2 - speed.y * time <= pos.y1) && (playerPos.y2 >= pos.y1))
            |> List.isEmpty) == False

upImpact speed time posList playerPos =
    (posList|> List.filter (projectionOverlap .x1 .x2 playerPos)
            |> List.filter (\pos -> (playerPos.y1 - speed.y * time >= pos.y2) && (playerPos.y1 <=pos.y2))
            |> List.isEmpty) == False


projectionOverlap min max pos1 pos2=
    (pos1 |> min) < (pos2 |> max) && (pos1 |> max) > (pos2 |> min)

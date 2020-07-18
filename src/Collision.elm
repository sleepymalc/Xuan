module Collision exposing (..)

rightImpact player time posList =
    (posList|> List.filter (projectionOverlap .y1 .y2 player.pos)
            |> List.filter (\pos -> (player.pos.x2 < pos.x1) && (player.pos.x2 + player.speed.x * time > pos.x1))
            |> List.isEmpty) == False

leftImpact player time posList =
    (posList|> List.filter (projectionOverlap .y1 .y2 player.pos)
            |> List.filter (\pos -> (player.pos.x1 > pos.x2) && (player.pos.x1 + player.speed.x * time < pos.x2))
            |> List.isEmpty) == False

downImpact player time posList =
    (posList|> List.filter (projectionOverlap .x1 .x2 player.pos)
            |> List.filter (\pos -> (player.pos.y2 < pos.y1) && (player.pos.y2 + player.speed.y * time >pos.y1))
            |> List.isEmpty) == False

upImpact player time posList =
    (posList|> List.filter (projectionOverlap .x1 .x2 player.pos)
            |> List.filter (\pos -> (player.pos.y1 > pos.y2) && (player.pos.y1 + player.speed.y * time < pos.y2))
            |> List.isEmpty) == False


projectionOverlap min max pos1 pos2=
    (pos1 |> min) < (pos2 |> max) && (pos1 |> max) > (pos2 |> min)

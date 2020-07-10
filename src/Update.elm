module Update exposing (..)
import Message exposing (..)
import Model exposing (..)
import Random

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetViewport { viewport } ->
            ( { model
                | size = Vector viewport.width viewport.height
              }
            , Cmd.none
            )
            
        Resize width height -> 
            ( { model 
                | size = Vector (toFloat width) (toFloat height )
                }
            , Cmd.none
            )

        Move moveDirection on->
            case moveDirection of
                Left -> 
                    ({model|
                        player=model.player |> run 
                    },Cmd.none)
                Right -> 
                    ({model|
                        player=model.player |> run 
                    },Cmd.none)

        Tick time ->
            case model.state of
                Playing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )

run player = player
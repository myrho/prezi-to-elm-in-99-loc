import Html
import Svg exposing (..)
import Svg.Attributes exposing (cx, cy, r, width, height, viewBox, fill)
import AnimationFrame
import Mouse

type Msg
    = Tick Float
    | Forth

duration =
    2000

update msg model =
  case msg of
    Forth ->
      { model
        | animation =
            List.head model.frames
              |> Maybe.map (\first -> (0,first))
        , frames =
            List.drop 1 model.frames
      }

    Tick diff ->
      case model.animation of
        Nothing ->
          model
        Just (oldProgress, target) ->
          if oldProgress == 1 then
            { model
              | animation = Nothing
            }
          else
            let
              newProgress =
                oldProgress + (diff / duration)
                  |> min 1

              newCurrent =
                { x = 
                    model.current.x + (target.x - model.current.x) 
                , y = 
                    model.current.y + (target.y - model.current.y)
                , width = 
                    model.current.width + (target.width - model.current.width) 
                , height = 
                    model.current.height + (target.height - model.current.height) 
                }
            in
              { model
                | current = newCurrent
                , animation = Just (newProgress, target)
              } 

view { current } =
  svg
    [ width "100vw"
    , height "100vh"
    , (toString current.x) ++ " "
        ++ (toString current.y) ++ " "
        ++ (toString current.width) ++ " "
        ++ (toString current.height)
        |> viewBox
    ]
    [ circle [ cx "100", cy "50", r "30", fill "blue" ] []
    ]

subscriptions model =
    Sub.batch
        [ case model.animation of
            Nothing ->
                Sub.none
            Just _ ->
                AnimationFrame.diffs Tick
        , Mouse.clicks (\_ -> Forth)
        ]

main =
    Html.program
        { init = (init, Cmd.none)
        , update = \msg model -> (update msg model, Cmd.none)
        , view = view
        , subscriptions = subscriptions
        }

init =
  { current = 
      { x = 0
      , y = 0
      , width = 150
      , height = 100
      }
  , animation = Nothing
  , frames = []
  }


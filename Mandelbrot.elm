module Mandelbrot exposing (..)

import Canvas exposing (Size, DrawOp(..))
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import Color exposing (Color)
import Color
import Window
import Task
import Html exposing (Html, program)
import Complex exposing (..)

type alias Model =
  { width : Int
  , height : Int
  }

init : ( Model, Cmd Msg )
init = ( { width = 10, height = 10 }, initCmd )

initCmd : Cmd Msg
initCmd =
  Task.perform (\size -> WindowResize  { width = size.width, height = size.height }) Window.size

type Msg
  = WindowResize Window.Size

view : Model -> Html Msg
view model =
    Canvas.initialize (Canvas.Size model.width model.height)
        |> Canvas.batch (fillBlue model)
        |> Canvas.toHtml []

--getValue : Int -> Int -> Int -> Int -> Int
--getValue width height x y =
--  Complex (-2 + (x / width)) (-2 + (x / height))

-- getColor : Int -> Int -> Int -> Int -> Color.Color
-- getColor width height x y =
--   Complex (-2 + (x / width)) (-2 + (x / height))

fillPixel : Int -> Int -> Int -> Int -> List DrawOp
fillPixel width height x y =
  [ FillStyle (Color.rgb (x * y) x y)
  , FillRect
    (Point.fromInts ( x, y ))
    (Canvas.Size 1 1)
  ]


fillBlue : Model -> List DrawOp
fillBlue model =
    List.map (\x -> List.map (\y -> fillPixel model.width model.height x y) (List.range 0 model.height) |> List.concat) (List.range 0 model.width) |> List.concat

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    WindowResize { width, height } ->
      ( { width = width, height = height }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Window.resizes WindowResize

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

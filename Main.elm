import Draggable as D exposing
  ( Draggable
  , Shape(..)
  , Direction(..)
  , updateHelp
  )

import Html exposing (Html, div)
import Html.App as App
import Mouse exposing (Position)

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { triangleUp : Draggable
  , triangleDown : Draggable
  , triangleLeft : Draggable
  , triangleRight : Draggable
  , triangleUpRight : Draggable
  , diamond : Draggable
  , parallelogram : Draggable
  }

init : (Model, Cmd Msg)
init =
  ( Model
      (Draggable (Position 300 100) Nothing (Triangle Up) 100 "skyblue")
      (Draggable (Position 500 100) Nothing (Triangle Down) 50 "orange")
      (Draggable (Position 700 100) Nothing (Triangle Left) 50 "orange")
      (Draggable (Position 100 100) Nothing (Triangle Right) 100 "slategray")
      (Draggable (Position 100 300) Nothing (Triangle UpRight) 100 "skyblue")
      (Draggable (Position 300 300) Nothing Diamond 100 "yellowgreen")
      (Draggable (Position 500 300) Nothing Parallelogram 100 "yellowgreen")
  , Cmd.none
  )

type Msg
  = TriangleRightMove D.Msg
  | TriangleUpMove D.Msg
  | TriangleDownMove D.Msg
  | TriangleLeftMove D.Msg
  | TriangleUpRightMove D.Msg
  | DiamondMove D.Msg
  | ParallelogramMove D.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TriangleUpMove submsg ->
      ({ model | triangleUp = updateHelp submsg model.triangleUp }, Cmd.none)
    TriangleDownMove submsg ->
      ({ model | triangleDown = updateHelp submsg model.triangleDown }, Cmd.none)
    TriangleLeftMove submsg ->
      ({ model | triangleLeft = updateHelp submsg model.triangleLeft }, Cmd.none)
    TriangleRightMove submsg ->
      ({ model | triangleRight = updateHelp submsg model.triangleRight }, Cmd.none)
    TriangleUpRightMove submsg ->
      ({ model | triangleUpRight = updateHelp submsg model.triangleUpRight }, Cmd.none)
    DiamondMove submsg ->
      ({ model | diamond = updateHelp submsg model.diamond }, Cmd.none)
    ParallelogramMove submsg ->
      ({ model | parallelogram = updateHelp submsg model.parallelogram }, Cmd.none)

view : Model -> Html Msg
view model =
  div
    []
    [ D.view model.triangleUp |> App.map TriangleUpMove
    , D.view model.triangleDown |> App.map TriangleDownMove
    , D.view model.triangleLeft |> App.map TriangleLeftMove
    , D.view model.triangleRight |> App.map TriangleRightMove
    , D.view model.triangleUpRight |> App.map TriangleUpRightMove
    , D.view model.diamond |> App.map DiamondMove
    , D.view model.parallelogram |> App.map ParallelogramMove
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ D.subscriptions model.triangleUp |> Sub.map TriangleUpMove
    , D.subscriptions model.triangleDown |> Sub.map TriangleDownMove
    , D.subscriptions model.triangleLeft |> Sub.map TriangleLeftMove
    , D.subscriptions model.triangleRight |> Sub.map TriangleRightMove
    , D.subscriptions model.triangleUpRight |> Sub.map TriangleUpRightMove
    , D.subscriptions model.diamond |> Sub.map DiamondMove
    , D.subscriptions model.parallelogram |> Sub.map ParallelogramMove
    ]

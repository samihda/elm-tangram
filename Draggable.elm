module Draggable exposing
  ( Draggable
  , Shape(..)
  , Direction(..)
  , Msg
  , view
  , subscriptions
  , updateHelp
  )

import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)

type alias Model =
  Draggable

type alias Draggable =
  { position : Position 
  , drag : Maybe Drag
  , shape : Shape
  , size : Int
  , color : String
  }

type alias Drag =
  { start : Position
  , current : Position
  }

type Shape
  = Diamond
  | Triangle Direction
  | Parallelogram

type Direction
  = Up
  | Down
  | Left
  | Right
  | UpRight

type Msg
  = DragStart Position
  | DragAt Position
  | DragEnd Position

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (updateHelp msg model, Cmd.none)

updateHelp : Msg -> Model -> Model
updateHelp msg ({ position, drag, shape, size, color } as model) =
  case msg of
    DragStart xy ->
      Draggable position (Just (Drag xy xy)) shape size color
    DragAt xy ->
      Draggable position (Maybe.map (\{ start } -> Drag start xy) drag) shape size color
    DragEnd _ ->
      Draggable (getPosition model) Nothing shape size color

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none
    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

view : Model -> Html Msg
view model =
  let
    realPosition =
      getPosition model
    styles =
      getStyles model
  in
    div
      [ onMouseDown
      , List.append
          styles
          [ "cursor" => "move"
          , "position" => "absolute"
          , "left" => px realPosition.x
          , "top" => px realPosition.y
          ]
        |> style
      ]
      []

getStyles : Draggable -> List (String, String)
getStyles draggable =
  case draggable.shape of
    Diamond ->
      diamond draggable
    Triangle direction ->
      triangle draggable direction
    Parallelogram ->
      parallelogram draggable

diamond : Draggable -> List (String, String)
diamond { size, color } =
  let
    leg =
      size ^ 2 // 2
        |> toFloat
        |> sqrt
        |> round
  in
    [ "background-color" => color
    , "width" => px leg
    , "height" => px leg
    , "transform" => "rotate(-45deg)"
    ]

triangle : Draggable -> Direction -> List (String, String)
triangle { size, color } direction =
  let
    transparent =
      px size ++ " solid transparent"
    solid =
      px size ++ " solid " ++ color
    transparentLeft =
      "border-left" => transparent
    transparentRight =
      "border-right" => transparent
    transparentTop =
      "border-top" => transparent
    transparentBottom =
      "border-bottom" => transparent
    solidTop =
      "border-top" => solid
    borderStyles =
      case direction of
        Up ->
          [ transparentLeft
          , transparentRight
          , "border-bottom" => solid
          ]
        Down ->
          [ transparentLeft
          , transparentRight
          , solidTop
          ]
        Left ->
          [ transparentTop
          , transparentBottom
          , "border-right" => solid
          ]
        Right ->
          [ transparentTop
          , transparentBottom
          , "border-left" => solid
          ]
        UpRight ->
          [ transparentLeft
          , solidTop
          ]
  in
    List.append
      borderStyles
      [ "width" => "0"
      , "height" => "0"
      ]

parallelogram : Draggable -> List (String, String)
parallelogram { size, color } =
  [ "background-color" => color
  , "width" => px size
  , "height" => px (size // 2)
  , "transform" => "skew(45deg)"
  ]

px : Int -> String
px number =
  toString number ++ "px"

(=>) : a -> b -> (a, b)
(=>) = (,)

getPosition : Model -> Position
getPosition { position, drag } =
  case drag of
    Nothing ->
      position
    Just { start, current } ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)

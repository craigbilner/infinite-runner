module Main (..) where

import StartApp
import Time exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Html exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Keyboard exposing (..)
import Debug exposing (..)


port currentTime : Int
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = inputs
    }


init : ( Model, Effects Action )
init =
  ( initModel, Effects.none )


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks



--Model


type alias Model =
  { x : Int
  , y : Int
  , isJumping : Bool
  }


initModel : Model
initModel =
  Model 0 0 False



--Update


type Action
  = UpdateTime Time
  | SpacebarDown Bool
  | NoOp


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    UpdateTime time ->
      let
        isJumping =
          if model.isJumping && model.y >= 0 && model.y < 300 then
            True
          else
            False

        jump =
          if isJumping then
            20
          else if model.y > 0 then
            -30
          else
            0

        obstacle =
          if model.x == -800 then
            0
          else
            model.x - 20

        newModel =
          { model
            | x = obstacle
            , y = model.y + jump
            , isJumping = isJumping
          }
      in
        ( newModel, Effects.none )

    SpacebarDown isDown ->
      let
        newModel =
          { model | isJumping = isDown || model.isJumping }
      in
        ( newModel, Effects.none )



--View


view : Signal.Address Action -> Model -> Html.Html
view address { x, y } =
  div
    []
    [ fromElement <| makePlayer x y ]


makePlayer : Int -> Int -> Element
makePlayer x y =
  collage
    1600
    800
    [ ngon 100 40
        |> filled clearGrey
        |> move ( -700, toFloat y )
    , ngon 4 50
        |> filled clearBlue
        |> rotate 90
        |> move ( toFloat x, 0 )
    ]


clearGrey : Color
clearGrey =
  rgba 111 255 111 0.6


clearBlue : Color
clearBlue =
  rgba 111 111 111 0.5


inputs =
  [ Signal.map UpdateTime <| fps 30
  , Signal.map SpacebarDown Keyboard.space
  ]

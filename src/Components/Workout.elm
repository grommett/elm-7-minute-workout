module Workout exposing (Model, model, view, update)
import Html exposing (Html, div)
import Html.Attributes exposing (..)

type alias Model =
  { count : Int }

type Msg
  = Pause
  | Toggle

model : Model
model =
  { count = 0

  }

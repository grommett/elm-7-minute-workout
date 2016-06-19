module Workout exposing (Msg, Model, model, view, update, start, updateCount, updateExercise)
import Html exposing (Html, div, text, button, h2, p)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model =
  { count : Int
  , viewState: Bool
  , exercise: String
  }

type alias Exercise =
  { id: Int
  , name: String
  }

type Msg
  = Pause
  | Stop
  | Count Int
  | Start Exercise
  | Change Exercise

model : Model
model =
  { count = 0
  , viewState = False
  , exercise = "No exercise"
  }

update: Msg -> Model -> Model
update msg model =
  case msg of
    Pause ->
      model
    Stop ->
      {model | viewState = False}
    Count int ->
      {model | count = int}
    Start record ->
      { model | exercise = record.name, viewState = True}
    Change record ->
      { model | exercise = record.name}

view: Model -> Html Msg
view model =
  div [id "exercise-workout-view", class (if model.viewState == True then "open" else "")]
  [ h2 [] [text model.exercise]
  , p [] [text (toString model.count)]
  , button [onClick Stop] [text "Stop"]
  ]

updateCount: Int -> Model -> Model
updateCount int model =
   update (Count int) model

updateExercise record model =
  update (Change record) model

start record model =
  update (Start record) model

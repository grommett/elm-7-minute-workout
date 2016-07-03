module Workout exposing (Msg, Model, model, view, update, start, updateCount, updateExercise, done)
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
  | Start Exercise Int
  | Change Exercise Int

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
    Start record duration ->
      { model | exercise = record.name, count = duration, viewState = True}
    Change record duration->
      { model | exercise = record.name, count = duration}

view: Model -> Html Msg
view model =
  let
    doneLabel = "Done"
    stopLabel = "Stop"
    count = toString model.count
  in
    div [id "exercise-workout-view", class (if model.viewState == True then "open" else "")]
    [ h2 [] [text model.exercise]
    , p [] [text (if(model.exercise == "Done") then "" else count)]
    , button [onClick Stop] [text (if model.exercise == "Done" then doneLabel else stopLabel)]
    ]

updateCount: Int -> Model -> Model
updateCount int model =
   update (Count int) model

updateExercise record duration model =
  update (Change record duration) model

start record duration model =
  update (Start record duration) model

done model =
  update Stop model

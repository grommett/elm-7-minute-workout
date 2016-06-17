module Main exposing (..)
import Html.App as App
import Html exposing (Html, div, p, button, text, h1, h2, section)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import ExerciseList exposing (..)
import ExerciseDescription exposing (..)
import Workout exposing (..)
import Timer exposing (..)
import Time exposing (Time, every, second)
import Debug

type Msg
  = SelectExercise ExerciseList.Msg
  | CloseDescription ExerciseDescription.Msg
  | CloseWorkout Workout.Msg
  | Tick Timer.Msg
  | Toggle

type ChildMsg = Set (ExerciseList.Msg)
type alias Model =
  { id : Int
  , exerciseListModel : ExerciseList.Model
  , exerciseDescriptionModel: ExerciseDescription.Model
  , started: Bool
  , displayDescription: Bool
  , timerModel : Timer.Model
  , workoutModel : Workout.Model
  , exerciseMetaView: String
  , mode: String
  }


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectExercise msg ->
      let
        newListModel = ExerciseList.update msg model.exerciseListModel
        exercise = newListModel.exercise
        newExerciseDesc = ExerciseDescription.updateExercise exercise
      in
        (
        { model |
          displayDescription = True
        , exerciseListModel = newListModel
        , exerciseDescriptionModel = newExerciseDesc
        , exerciseMetaView = "view-exercise-description-view"
        }
        , Cmd.none)

    CloseDescription msg ->
      ({ model |
         displayDescription = False
       , exerciseDescriptionModel = ExerciseDescription.update msg model.exerciseDescriptionModel
       , exerciseMetaView = ""
       }, Cmd.none)

    CloseWorkout msg ->
      ({model | workoutModel = Workout.update msg model.workoutModel }, Cmd.none)

    Tick msg ->
      {--
      If in rest mode and not eq 0 -> Tick
      if in rest mode and eq 0 -> get next exercise and set to 
       --}
      ({ model |
         timerModel = Timer.update msg model.timerModel
       , workoutModel = Workout.updateCount model.timerModel.countdown model.workoutModel
       } , Cmd.none)

    Toggle ->
      let
        newWorkout = Workout.toggle model.workoutModel
      in
        ({ model |
            timerModel = (Timer.toggle model.timerModel)
          , workoutModel = newWorkout
          }, Cmd.none)

model: (Model, Cmd Msg)
model =
  ({ id = 0
  , exerciseListModel = ExerciseList.model
  , started = False
  , displayDescription = False
  , exerciseDescriptionModel = ExerciseDescription.model
  , workoutModel = Workout.model
  , timerModel = Timer.model
  , exerciseMetaView = ""
  , mode = "list"
  }, Cmd.none)

view: Model -> Html Msg
view model =
  Html.div [id "app-container"]
  [p [] [text (toString model)]
   , div [id "exercise-meta", class model.exerciseMetaView]
     [ App.map SelectExercise (ExerciseList.view model.exerciseListModel)
     , App.map CloseDescription (ExerciseDescription.view model.exerciseDescriptionModel)
     ]
   , App.map CloseWorkout (Workout.view model.workoutModel)
   , button [onClick Toggle] [ text (if model.timerModel.paused then "Start" else "Pause")]
  ]

subscription: Model -> Sub Msg
subscription model =
  Sub.map Tick (Timer.subscription model.timerModel)

main: Program Never
main =
  App.program {subscriptions = subscription, init=model, update=update, view=view}

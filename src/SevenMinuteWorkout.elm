module SevenMinuteWorkout exposing (..)
import Html.App as App
import Html exposing (Html, div, p, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import ExerciseList exposing (..)
import ExerciseDescription exposing (..)
import Workout exposing (..)
import Timer exposing (..)

type Msg
  = SelectExercise ExerciseList.Msg
  | CloseDescription ExerciseDescription.Msg
  | CloseWorkout Workout.Msg
  | Tick Timer.Msg
  | Start

type alias Model =
  { currentTime: Int
  , viewState: String
  , exerciseMetaView: String
  , exerciseId: Int
  , exerciseList : ExerciseList.Model
  , exerciseDesc: ExerciseDescription.Model
  , timer : Timer.Model
  , workout : Workout.Model
  }

model: (Model, Cmd Msg)
model =
  ({ currentTime = 0
  , viewState = "loading"
  , exerciseMetaView = ""
  , exerciseId = 0
  , exerciseList = ExerciseList.model
  , exerciseDesc = ExerciseDescription.model
  , timer = Timer.model
  , workout = Workout.model
  }, Cmd.none)

view: Model -> Html Msg
view model =
  Html.div [id "app-container"]
  [p [] [text (toString model)]
   , div [id "exercise-meta", class model.exerciseMetaView]
     [ App.map SelectExercise (ExerciseList.view model.exerciseList)
     , App.map CloseDescription (ExerciseDescription.view model.exerciseDesc)
     ]
   , App.map CloseWorkout (Workout.view model.workout)
   , button [onClick Start] [text "Start"]
  ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectExercise msg ->
      let
        exercise = ExerciseList.update msg model.exerciseList
        newExerciseDesc = ExerciseDescription.updateExercise exercise.exercise
      in
        ({ model
         | viewState = "description"
         , exerciseList = exercise
         , exerciseDesc = newExerciseDesc
         , exerciseId = exercise.exerciseId
         , exerciseMetaView = "view-exercise-description-view"
         }, Cmd.none)

    CloseDescription msg ->
      ({model | viewState = "list", exerciseMetaView= ""}, Cmd.none)

    CloseWorkout msg ->
      let
        newWorkout = Workout.update msg model.workout
      in
        ({ model
         | workout = newWorkout
         , timer = (Timer.toggle model.timer)
         }, Cmd.none)

    Tick msg ->
      let
        newTimer = Timer.update msg model.timer
        currentCount = newTimer.countdown
        newWorkout = Workout.updateCount currentCount model.workout
      in
        ({ model
         | timer = newTimer
         , workout = newWorkout
         } , Cmd.none)

    Start ->
      let
        startedWorkout = Workout.start (getExerciseById 0 model.exerciseList) model.workout
        newTimer = Timer.setCountdown model.timer 5
        newWorkout = Workout.updateCount newTimer.countdown startedWorkout
      in
        ({ model
         | viewState = "exercise"
         , workout = newWorkout
         , timer = (Timer.toggle newTimer)
         }, Cmd.none)

getExerciseById: Int -> ExerciseList.Model -> { id : Int, name : String }
getExerciseById int model =
  let
    exercise = ExerciseList.getExercise int model
  in
    case exercise of
      Just value ->
        {id = value.id, name = value.name}
      Nothing ->
        {id = -1, name = "Done"}

subscription: Model -> Sub Msg
subscription model =
  Sub.map Tick (Timer.subscription model.timer)

main: Program Never
main =
  App.program {subscriptions = subscription, init=model, update=update, view=view}

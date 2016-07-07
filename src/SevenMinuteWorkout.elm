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
  , exerciseId: Int
  , exerciseList : ExerciseList.Model
  , exerciseDesc: ExerciseDescription.Model
  , timer : Timer.Model
  , workout : Workout.Model
  }

restDuration : Int
restDuration
  = 5

exerciseDuration: Int
exerciseDuration
  = 8

debug: Bool
debug = False

model: (Model, Cmd Msg)
model =
  ({ currentTime = 0
  , viewState = "loading"
  , exerciseId = 0
  , exerciseList = ExerciseList.model
  , exerciseDesc = ExerciseDescription.model
  , timer = Timer.model
  , workout = Workout.model
  }, Cmd.none)

view: Model -> Html Msg
view model =
  div [id "app-container"]
  [p [] [text (if debug then toString model else "")]
   , App.map SelectExercise (ExerciseList.view model.exerciseList)
   , App.map CloseDescription (ExerciseDescription.view model.exerciseDesc)
   , App.map CloseWorkout (Workout.view model.workout)
   , button [onClick Start] [text "Start"]
  ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectExercise msg ->
      let
        exerciseList = ExerciseList.update msg model.exerciseList
      in
        ({ model
         | viewState = "description"
         , exerciseList = exerciseList
         , exerciseDesc = ExerciseDescription.updateExercise { viewState=True, exercise = exerciseList.exercise }
         }, Cmd.none)

    CloseDescription msg ->
      let
        exeDesc = ExerciseDescription.update msg model.exerciseDesc
        exeList = ExerciseList.open model.exerciseList
      in
        ({model | viewState = "list", exerciseDesc= exeDesc, exerciseList = exeList}, Cmd.none)

    CloseWorkout msg ->
      ({ model
         | workout = Workout.update msg model.workout
         , timer = (Timer.toggle model.timer)
         }, Cmd.none)

    Tick msg ->
      let
        newTimer = Timer.update msg model.timer
        currentCount = newTimer.countdown
        newWorkout = Workout.updateCount currentCount model.workout
        timerWorkout = nextExerciseState newTimer newWorkout model currentCount
      in
        ({ model
         | timer = timerWorkout.timer
         , workout = timerWorkout.workout
         , exerciseId = timerWorkout.exerciseId
         } , Cmd.none)

    Start ->
      let
        newWorkout = Workout.start (getExerciseById 0 model.exerciseList) exerciseDuration model.workout
        newTimer = Timer.setCountdown model.timer exerciseDuration
      in
        ({ model
         | viewState = "exercise"
         , workout = newWorkout
         , timer = (Timer.toggle newTimer)
         , exerciseId = 0
         }, Cmd.none)

getExerciseById: Int -> ExerciseList.Model -> { id : Int, name : String }
getExerciseById int model =
  let
    exercise = ExerciseList.getExercise int model
  in
    case exercise of
      Just value ->
        { id = value.id, name = value.name }
      Nothing ->
        { id = -1, name = "Done" }

nextExerciseState: Timer.Model -> Workout.Model -> Model -> Int -> {exerciseId: Int, timer: Timer.Model, workout: Workout.Model}
nextExerciseState timer workout model count =
  let
    exerciseId = model.exerciseId + 1
    lastWorkout = workout.exercise
    nextWorkout = Workout.updateExercise (getExerciseById exerciseId model.exerciseList) exerciseDuration workout
    exerciseTimer = Timer.setCountdown model.timer exerciseDuration
    restTimer = Timer.setCountdown model.timer restDuration
    restWorkout = Workout.updateExercise {id=-1, name="Rest"} restDuration workout
  in
    if count < 0 && nextWorkout.exercise == "Done" then
      { exerciseId = model.exerciseId, timer = timer, workout = nextWorkout }
    else if count < 0 && lastWorkout == "Rest" then
      { exerciseId = exerciseId, timer = exerciseTimer, workout = nextWorkout }
    else if count < 0 && lastWorkout /= "Rest" then
      { exerciseId = model.exerciseId, timer = restTimer, workout = restWorkout }
    else
      { exerciseId = model.exerciseId, timer = timer, workout = workout }

subscription: Model -> Sub Msg
subscription model =
  Sub.map Tick (Timer.subscription model.timer)

main: Program Never
main =
  App.program {subscriptions = subscription, init=model, update=update, view=view}

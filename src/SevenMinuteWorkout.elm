module SevenMinuteWorkout exposing (..)
import Html.App as App
import Html exposing (Html, div, p, button, text, pre)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import ExerciseList as ExerciseList exposing (..)
import ExerciseDescription exposing (..)
import Workout exposing (..)
import Timer exposing (..)
import Exercise exposing (Exercise)
type Msg
  = ExerciseListMsgs ExerciseList.Msg
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

-- Main --

main : Program Never
main =
  App.program
    { subscriptions = subscription
    , init = init
    , update = update
    , view = view
    }


-- Model --


model : ExerciseList.Model -> Model
model exeList =
  { currentTime = 0
  , viewState = "loading"
  , exerciseId = -1
  , exerciseList = exeList
  , exerciseDesc = ExerciseDescription.model
  , timer = Timer.model
  , workout = Workout.model
  }

init : (Model, Cmd Msg)
init =
  let
    (exerciseList, eCmd) = ExerciseList.init
  in
    (model exerciseList) ! [ Cmd.map ExerciseListMsgs eCmd ]


-- Update --


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ExerciseListMsgs eMsg ->
      let
        (exerciseList, cmd) =
          ExerciseList.update eMsg model.exerciseList
        exerciseDesc =
          ExerciseDescription.updateExercise exerciseList.exercise model.exerciseDesc
      in
        { model | exerciseList = exerciseList, exerciseDesc = exerciseDesc } ! [Cmd.map ExerciseListMsgs cmd]

    CloseDescription msg ->
      let
        exerciseDesc =
          ExerciseDescription.update msg model.exerciseDesc
        (exerciseList, cmd) =
          ExerciseList.open model.exerciseList
      in
        ({ model | exerciseDesc = exerciseDesc, exerciseList = exerciseList }, Cmd.none)

    CloseWorkout msg ->
      ({ model
         | workout = Workout.update msg model.workout
         , timer = (Timer.toggle model.timer)
         },
       Cmd.none
       )

    Start ->
      let
        exercise = ExerciseList.getExercise 0 model.exerciseList.exercises
        newWorkout = Workout.start exercise exerciseDuration model.workout
        newTimer = Timer.setCountdown model.timer exerciseDuration
      in
        ({ model
         | viewState = "exercise"
         , workout = newWorkout
         , timer = (Timer.toggle newTimer)
         , exerciseId = 0
         }, Cmd.none)

    Tick msg ->
      let
        newTimer = Timer.update msg model.timer
        currentCount = newTimer.countdown
        newWorkout = Workout.updateCount currentCount model.workout
        nextWorkout = getNextWorkout model newWorkout newTimer currentCount
      in
        ({ model
         | timer = nextWorkout.timer
         , workout = nextWorkout.workout
         , exerciseId = nextWorkout.exerciseId
         } , Cmd.none)

getNextWorkout : Model -> Workout.Model -> Timer.Model -> Int -> { exerciseId: Int, timer: Timer.Model, workout: Workout.Model }
getNextWorkout model workout timer count =
  let
    exerciseId = model.exerciseId + 1
    exerciseTimer = Timer.setCountdown timer exerciseDuration
    restTimer = Timer.setCountdown timer restDuration
    lastWorkout = workout.exercise
    nextExercise = ExerciseList.getExercise exerciseId model.exerciseList.exercises
    rest = Exercise "Rest" "gray" -1 ""
    done = Exercise "Done" "gray" -1 ""
    nextWorkout = Workout.updateExercise nextExercise exerciseDuration workout
    restWorkout = Workout.updateExercise rest restDuration workout
    doneWorkout = Workout.updateExercise done restDuration workout
  in
    if count < 0 && nextWorkout.exercise.name == "" then
      { exerciseId = model.exerciseId, timer = timer, workout = doneWorkout }
    else if count < 0 && lastWorkout.name == "Rest" then
      { exerciseId = exerciseId, timer = exerciseTimer, workout = nextWorkout }
    else if count < 0 && lastWorkout.name /= "Rest" then
      { exerciseId = model.exerciseId, timer = restTimer, workout = restWorkout }
    else
      { exerciseId = model.exerciseId, timer = timer, workout = workout }

-- View --

view : Model -> Html Msg
view model =
  div [id "app-container"]
  [  App.map ExerciseListMsgs <| ExerciseList.view model.exerciseList
   , App.map CloseDescription <| ExerciseDescription.view model.exerciseDesc
   , App.map CloseWorkout <| Workout.view model.workout
   , button [class "start-btn", onClick Start] [text "Start"]
   --, p [] [ text (toString model) ]
  ]

-- Subscription --

subscription : Model -> Sub Msg
subscription model =
  Sub.batch
    [
    Sub.map Tick (Timer.subscription model.timer)
    ]

-- Set Up --

restDuration : Int
restDuration
  = 10

exerciseDuration : Int
exerciseDuration
  = 30

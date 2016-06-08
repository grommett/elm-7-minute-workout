module Main exposing (..)
import Html.App as App
import Html exposing (Html, div, p, button, text, h1, h2, section)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import ExerciseList exposing (..)
import ExerciseDescription exposing (..)
import Timer exposing (..)
import Time exposing (Time, every, second)
import Debug

type Msg
  = SelectExercise ExerciseList.Msg
  | CloseDescription ExerciseDescription.Msg
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
  , exerciseMetaView: String
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

    Tick msg ->
      ({model | timerModel = Timer.update msg model.timerModel}, Cmd.none)

    Toggle ->
      ({model | timerModel = (Timer.toggle model.timerModel)}, Cmd.none)

model: (Model, Cmd Msg)
model =
  ({ id = 0
  , exerciseListModel = ExerciseList.model
  , started = False
  , displayDescription = False
  , exerciseDescriptionModel = ExerciseDescription.model
  , timerModel = Timer.model
  , exerciseMetaView = ""
  }, Cmd.none)

view: Model -> Html Msg
view model =
  Html.div [id "app-container"]
  [p [] [text (toString model)]
   , div [id "exercise-meta", class model.exerciseMetaView]
     [ App.map SelectExercise (ExerciseList.view model.exerciseListModel)
     , App.map CloseDescription (ExerciseDescription.view model.exerciseDescriptionModel)
     ]
   , button [onClick Toggle] [ text (if model.timerModel.paused then "Resume" else "Pause")]
  ]

subscription: Model -> Sub Msg
subscription model =
  Sub.map Tick (Timer.subscription model.timerModel)

main: Program Never
main =
  App.program {subscriptions = subscription, init=model, update=update, view=view}

module ExerciseList exposing (Model, Msg(..), Exercise, model, view, update, getExercise)
import Html exposing (Html, div, p, ul, li, button, text, section, h1, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App

type Msg
  = Set Exercise
  | Select Exercise

type alias Exercise =
  { id: Int
  , name: String
  , description: String
  , videoId: Int
  }


type alias Model =
  { exercises: List Exercise
  , exerciseId: Int
  , exercise: Exercise
  }

currentExercise =
  model.exercise

update: Msg -> Model -> Model
update msg model =
  case msg of
    Set record ->
      {model| exercise = record, exerciseId = record.id }
    Select record ->
      {model| exercise = record, exerciseId = record.id }

defaultExercise: Exercise
defaultExercise = Exercise 0 "PushUps" "An exercise" 123

model: Model
model =
  { exercises =
    [ defaultExercise
    , Exercise 1 "SitUps" "An exercise that is situps" 456
    , Exercise 2 "Wall Sits" "An exercise that is Wall Sits" 789
    ]
  , exerciseId = -1
  , exercise = defaultExercise
  }

filterExercises: Int -> Exercise -> Bool
filterExercises id exercise =
  exercise.id == id

getExercise: Int -> Model -> Maybe Exercise
getExercise id model =
  List.head (List.filter (filterExercises id) model.exercises)

exerciseListItem: Int -> Exercise -> Html Msg
exerciseListItem num exercise =
  li [ onClick (Select exercise), class "exercise-item" ]
    [ span [class "exercise-item-number"] [text (toString num)]
    , text exercise.name
    , span [ class "exercise-item-arrow"] [ text "›"]
    ]

exercises: List Exercise -> List (Html Msg)
exercises exercises =
  let
    num = 0
    dom = List.indexedMap (\int exe -> exerciseListItem (int+1) exe) exercises
  in
    dom

view: Model -> Html Msg
view model =
  section [id "exercise-list", class "exercise-meta-view view-exercise-view"]
  [ h1 [] [text "The 7 Minute Workout"]
  , p [] [text "Lorem ipsum dolor sit amet, consectetur adipisicing elit."]
  , ul []
      (exercises model.exercises)
  ]

main = view model

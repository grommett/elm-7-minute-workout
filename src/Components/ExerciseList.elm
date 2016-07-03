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
  , color: String
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
defaultExercise = Exercise 0 "Jumping Jacks" "An exercise" 123 "red"

model: Model
model =
  { exercises =
    [ defaultExercise
    , Exercise 1 "Wall Sits" "An exercise that is wall sits" 456 "gray"
    , Exercise 2 "Push-ups" "An exercise that is push-ups" 789 "blue"
    , Exercise 3 "Abdominal crunches" "An exercise that is abdominal crunches" 789 "darkBlue"
    , Exercise 4 "Step-ups onto a chair" "An exercise that is step-ups onto a chair" 789 "green"
    , Exercise 5 "Squats" "An exercise that is squats" 789 "purple"
    , Exercise 6 "Tricep dips on a chair" "An exercise that is tricep dips on a chair" 789 "lime"
    , Exercise 7 "Planks" "An exercise that is planks" 789 "orange"
    , Exercise 8 "High knees running in place" "An exercise that is high knees running in place" 789 "red"
    , Exercise 9 "Lunges" "An exercise that is lunges" 789 "gray"
    , Exercise 10 "Push-ups and rotations" "An exercise that is push-ups and rotations" 789 "blue"
    , Exercise 11 "Side planks" "An exercise that is side planks" 789 "darkBlue"
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
  li [ onClick (Select exercise), class ("exercise-item " ++ exercise.color) ]
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
  --, button[] [text "Start"]
  ]

main = view model

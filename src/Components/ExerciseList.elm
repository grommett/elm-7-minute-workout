module ExerciseList exposing (Model, Msg(..), Exercise, model, view, update, getExercise, open)
import Html exposing (Html, div, p, ul, li, button, text, section, h1, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App

type Msg
  = Set Exercise
  | Select Exercise
  | Open

type alias Exercise =
  { id: Int
  , name: String
  , description: String
  , videoId: String
  , color: String
  }


type alias Model =
  { viewState : Bool
  , exercises: List Exercise
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
      {model| exercise = record, exerciseId = record.id, viewState = False }
    Open ->
      {model| viewState = True }

defaultExercise: Exercise
defaultExercise = Exercise 0 "Jumping Jacks" "An exercise" "1b98WrRrmUs" "red"

model: Model
model =
  { viewState = True
  , exercises =
    [ defaultExercise
    , Exercise 1 "Wall Sits" "An exercise that is wall sits" "0RGCezLuP6c" "gray"
    , Exercise 2 "Push-ups" "An exercise that is push-ups" "dAfVhTXDUNo" "blue"
    , Exercise 3 "Abdominal crunches" "An exercise that is abdominal crunches" "_YVhhXc2pSY" "darkBlue"
    , Exercise 4 "Step-ups onto a chair" "An exercise that is step-ups onto a chair" "dG75KOf4EtY" "green"
    , Exercise 5 "Squats" "An exercise that is squats" "YYHDXY26GjE" "purple"
    , Exercise 6 "Tricep dips on a chair" "An exercise that is tricep dips on a chair" "3ydgLFLK8e0" "lime"
    , Exercise 7 "Planks" "An exercise that is planks" "JEkxJKCPiFU" "orange"
    , Exercise 8 "High knees running in place" "An exercise that is high knees running in place" "_a29JwDaVJw" "red"
    , Exercise 9 "Lunges" "An exercise that is lunges" "UWgWxKKdycU" "gray"
    , Exercise 10 "Push-ups with rotations" "An exercise that is push-ups and rotations" "YU0gWh72a3k" "blue"
    , Exercise 11 "Side planks" "An exercise that is side planks" "IkMmABQ9SkM" "darkBlue"
    ]
  , exerciseId = -1
  , exercise = defaultExercise
  }

filterExercises: Int -> Exercise -> Bool
filterExercises id exercise =
  exercise.id == id

open: Model -> Model
open model =
  update Open model

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
  let
    viewClass = if model.viewState == True then " open" else " closed"
  in
    section [id "exercise-list", class ("exercise-meta-view" ++ viewClass)]
    [ h1 [] [text "The 7 Minute Workout"]
    , p [] [text "Lorem ipsum dolor sit amet, consectetur adipisicing elit."]
    , ul []
        (exercises model.exercises)
  ]

main = view model

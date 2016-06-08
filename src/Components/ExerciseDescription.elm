module ExerciseDescription exposing (Model, Msg, updateExercise, view, update, model)
import Html exposing (Html, div, p, text, br, button, h2, section, header)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

type Msg =
  Close
  | Open
  | Change Model

type alias Model =
  { exercise:
    { id: Int
    , name: String
    , description: String
    , videoId: Int
    }
  , closed: Bool
  }

updateExercise record =
  let
    newModel = {model | exercise = record}
  in
    update (Change newModel) model

model: Model
model =
  { exercise =
    { id = 0
    , name = ""
    , description = ""
    , videoId = -1
    }
  , closed = True
  }


update: Msg -> Model -> Model
update msg model =
  case msg of
    Close ->
      {model | closed = True}

    Open ->
      {model | closed = False}

    Change newExerciseModel ->
      { model | exercise = newExerciseModel.exercise, closed = newExerciseModel.closed }

view: Model -> Html Msg
view model =
  section [id "exercise-description", class "exercise-meta-view"]
  [ header []
    [ h2 [] [text model.exercise.name]
    ,div [onClick Close, class "exercise-description-close"]
     [text "✖"]
    ]
    , p[][text model.exercise.description]
  ]

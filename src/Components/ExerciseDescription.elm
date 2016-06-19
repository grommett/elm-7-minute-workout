module ExerciseDescription exposing (Model, Msg, updateExercise, view, update, model)
import Html exposing (Html, div, p, text, br, button, h2, section, header)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

type Msg =
  Close
  | Change Model

type alias Model =
  { id: Int
  , name: String
  , description: String
  , videoId: Int
  }

updateExercise record =
  update (Change record) model

model: Model
model =
  { id = -1
  , name = ""
  , description = ""
  , videoId = -1
  }

update: Msg -> Model -> Model
update msg model =
  case msg of
    Close ->
      model

    Change newExerciseModel ->
      newExerciseModel

view: Model -> Html Msg
view model =
  section [id "exercise-description", class "exercise-meta-view"]
  [ header []
    [ h2 [] [text model.name]
    ,div [onClick Close, class "exercise-description-close"]
     [text "✖"]
    ]
    , p[][text model.description]
  ]

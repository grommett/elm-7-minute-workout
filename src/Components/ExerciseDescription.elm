module ExerciseDescription exposing (Model, Msg, updateExercise, view, update, model)
import Html exposing (Html, div, p, text, br, button, h2, section, header, iframe)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

type Msg =
  Close
  | Change Model

type alias Model =
  { viewState : Bool
  , exercise :
      { id: Int
      , name: String
      , description: String
      , videoId: String
      , color: String
      }
  }

youtubeUrl: String -> String
youtubeUrl id =
  "https://www.youtube.com/embed/" ++ id

updateExercise record =
  update (Change record) model

model: Model
model =
  { viewState = False
  , exercise =
      { id = -1
      , name = ""
      , description = ""
      , videoId = ""
      , color = ""
      }
  }

update: Msg -> Model -> Model
update msg model =
  case msg of
    Close ->
      {model | viewState = False}

    Change newExerciseModel ->
      newExerciseModel

view: Model -> Html Msg
view model =
  let
    viewClass = if(model.viewState == True) then " open" else ""
  in
    section [id "exercise-description", class ("exercise-meta-view" ++ viewClass ++ " " ++ model.exercise.color)]
    [ header []
      [ h2 [] [text model.exercise.name]
      , div [onClick Close, class "exercise-description-close"]
        [text "✖"]
      ]
      , div [class "video-container"]
        [
          if model.viewState == True then
            iframe [class "video", src (youtubeUrl model.exercise.videoId)] []
          else
            p[] [text ""]
        ]
      , p[][text model.exercise.description]
    ]

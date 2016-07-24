module ExerciseDescription exposing (Model, Msg, updateExercise, view, update, model)
import Exercise exposing (Exercise)
import Html exposing (Html, div, p, text, br, button, h2, section, header, iframe)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

type Msg =
  Close
  | Change Exercise

type alias Model =
  { viewState : Bool
  , exercise : Exercise
  }


youtubeUrl: String -> String
youtubeUrl id =
  "https://www.youtube.com/embed/" ++ id

updateExercise : Maybe Exercise -> Model -> Model
updateExercise newExercise model =
  case newExercise of
    Just value ->
      update (Change value) model
    Nothing ->
      model

model : Model
model =
  { viewState = False
  , exercise = Exercise "No exercise" "red" -1 "-1"
  }

update: Msg -> Model -> Model
update msg model =
  case msg of
    Close ->
      let
        current = model.exercise
        newExercise = { current | id = -1 }
      in
        { model | viewState = False, exercise = newExercise }

    Change newExercise ->
      { model | viewState = True, exercise = newExercise }

view: Model -> Html Msg
view model =
  let
    viewClass = if (model.viewState == True) then " open" else ""
  in
    section [id "exercise-description", class ("exercise-meta-view" ++ viewClass ++ " " ++ model.exercise.color)]
    [ header []
      [ h2 [] [text model.exercise.name ]
      , div [ onClick Close, class "exercise-description-close" ]
        [ text "✖" ]
      ]
      , div [ class "video-container" ]
        [
          if model.viewState == True then
            iframe [ class "video", src (youtubeUrl model.exercise.videoId) ] []
          else
            text ""
        ]
    ]

main =
  view model

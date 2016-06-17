module Workout exposing (Msg, Model, model, view, update, toggle, updateCount)
import Html exposing (Html, div, text, button, h2, p)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model =
  { count : Int
  , viewState: Bool
  }

type Msg
  = Pause
  | Toggle
  | Count Int

model : Model
model =
  { count = 30
  , viewState = False
  }

update: Msg -> Model -> Model
update msg model =
  case msg of
    Pause ->
      model
    Toggle ->
      {model | viewState = (not model.viewState)}
    Count int ->
      {model | count = int}

view: Model -> Html Msg
view model =
  div [id "exercise-workout-view", class (if model.viewState then "open" else "")]
  [ h2 [] [text "The Exercise"]
  , p [] [text (toString model.count)]
  , button [onClick Toggle] [text "Stop"]
  ]

toggle: Model -> Model
toggle model =
   update Toggle model

updateCount: Int -> Model -> Model
updateCount int model =
   update (Count int) model

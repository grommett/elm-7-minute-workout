module Workout exposing (Msg, Model, model, view, update, start, updateCount, updateExercise, done)
import Html exposing (Html, div, section, text, button, h2, p)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing(..)
import Svg.Attributes exposing(..)
import Exercise exposing (Exercise)
type alias Model =
  { count : Int
  , viewState : Bool
  , exercise : Exercise
  , duration : Int
  }


type Msg
  = Pause
  | Stop
  | Count Int
  | Start Exercise Int
  | Change Exercise Int

-- Model --

model : Model
model =
  { count = 0
  , viewState = False
  , exercise = Exercise "No exercise" "gray" -1 ""
  , duration = 0
  }

-- Update --

update: Msg -> Model -> Model
update msg model =
  case msg of
    Pause ->
      model

    Stop ->
      { model | viewState = False, count = 0, duration = 0}

    Count count ->
      { model | count = count }

    Start exercise duration ->
      { model | exercise = exercise, count = duration, duration = duration, viewState = True }

    Change exercise duration->
      { model | exercise = exercise, count = duration, duration = duration}

-- View --

view: Model -> Html Msg
view model =
  let
    doneLabel = "Done"
    stopLabel = "Stop"
    count = toString model.count
    exercise = model.exercise
    remaining = model.duration - model.count
    percentComplete = if model.duration /= 0 then (toFloat remaining) / (toFloat model.duration) else 0
    attributes = circleAttributes percentComplete
    attributesBkg = circleAttributes 1
  in
    section [ Html.Attributes.id "exercise-workout-view", Html.Attributes.class (if model.viewState == True then ("open " ++ exercise.color) else "") ]
    [ h2 [] [ Html.text exercise.name ]
    , div [ Html.Attributes.class "counter"]
      [ div [ Html.Attributes.class "count"] [ Html.text (if exercise.name == "Done" then "" else count) ]
      , div [ Html.Attributes.class "percentage" ] []
      , Svg.svg [Svg.Attributes.width "330", Svg.Attributes.height "330"]
        [ Svg.circle (Svg.Attributes.class "circle-bkg" :: attributesBkg) []
        , Svg.circle (Svg.Attributes.class "circle-percent" ::attributes) []
        ]
      ]
    , button [ onClick Stop ] [ Html.text (if exercise.name == "Done" then doneLabel else stopLabel) ]
    ]

circleAttributes : Float -> List (Svg.Attribute a)
circleAttributes percentComplete =
  let
    stroke = 30
    halfStroke = stroke/2
    radius = 150
  in
    [ Svg.Attributes.transform ("translate(" ++ (toString halfStroke) ++ "," ++ (toString halfStroke) ++ ") rotate(-90, 150, 150)")
    , Svg.Attributes.cx "150"
    , Svg.Attributes.cy "150"
    , Svg.Attributes.r (toString radius)
    , Svg.Attributes.strokeWidth (toString stroke)
    , Svg.Attributes.fill "none"
    , Svg.Attributes.strokeDasharray (getDashArray 150 percentComplete)
    ]

getDashArray : Int -> Float -> String
getDashArray radius percent =
  let
    dashLength = toString (percent * 2 * pi * radius)
    dashMax = toString (7 * radius)
  in
    dashLength ++ " " ++ dashMax

-- API --
updateCount: Int -> Model -> Model
updateCount int model =
   update (Count int) model

updateExercise exercise duration model =
  update (Change exercise duration) model

start: Exercise -> Int -> Model -> Model
start exercise duration model =
  update (Start exercise duration) model

done model =
  update Stop model

main =
  view model

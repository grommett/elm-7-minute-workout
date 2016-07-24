module ExerciseList exposing (Model, Msg(..), init, view, update, getExercise, open)
import Exercise exposing (Exercise)
import Html exposing (Html, div, p, ul, li, button, text, section, h1, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Http
import Json.Decode as Json exposing(..)
import Task


main =
  App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

type Msg
  = GetExercises
  | Open
  | Select Exercise
  | FetchSucceed (List (String, ExerciseProps))
  | FetchFail Http.Error

type alias Exercises =
  List Exercise

type alias Model =
  { exercises: Exercises
  , exercise: Maybe Exercise
  , viewState: String
  , errorMessage: String
  }

type alias ExerciseProps =
  { color: String
  , id: Int
  , videoId: String
  }

-- Model --

init: (Model, Cmd Msg)
init =
  (Model [] Nothing "Loading" "") ! [ getExerciseList ]

emptyExercise: Exercise
emptyExercise =
  Exercise "" "" -1 ""

-- HTTP --

getExerciseList : Cmd Msg
getExerciseList =
  let
    url =
      "//testing-35f49.firebaseio.com/exercises.json"
    log = Debug.log "fetch" "getting list!"
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeExerciseList url)

-- Update --

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetExercises ->
      let
        log = Debug.log "geting exercises" msg
      in
        { model | viewState = "Loading" } ! [ getExerciseList ]

    Select exercise ->
      ({ model | exercise = Just exercise, viewState="closed" }, Cmd.none)

    Open ->
      ({ model | viewState="open" }, Cmd.none)

    FetchSucceed exercises ->
      let
        sorted = List.map tupleToRecord ({--List.take 4 <| --}List.sortWith sortById exercises)
        log = Debug.log "FetchSucceed" sorted
      in
        ({model | exercises = sorted, viewState="Loaded", exercise = Nothing }, Cmd.none)

    FetchFail error ->
      let
        errorMessage = "Error loading the exercises!"
        log = Debug.log "error" error
      in
        (Model [] Nothing "Error" errorMessage, Cmd.none)

-- View --

view: Model -> Html Msg
view model =
  let
    viewClass =
      if model.viewState == "closed" then
        " closed"
      else
        " open"
  in
    section [ id "exercise-list", class ("exercise-meta-view" ++ viewClass) ]
    [ h1 [] [ text "The 7 Minute Workout" ]
    , p [] [ text "Lorem ipsum dolor sit amet, consectetur adipisicing elit." ]
    , ul []
        <| List.indexedMap (\int exercise -> exerciseListItem int exercise)  model.exercises
  ]

exerciseListItem: Int -> Exercise -> Html Msg
exerciseListItem num exercise =
  let
    delay = num * 1
    delayString = (toString delay) ++ "s"
  in
    li [ onClick (Select exercise), class ("exercise-item " ++ exercise.color), style[("transition-delay", delayString)] ]
      [ span [ class "exercise-item-number"] [ text (toString num) ]
      , text exercise.name
      , span [ class "exercise-item-arrow"] [ text "›" ]
      ]

-- Subscriptions --

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- Decoders --

decodeExercise : Decoder ExerciseProps
decodeExercise =
  object3 ExerciseProps
      ("color" := string)
      ("id" := int)
      ("videoId" := string)

decodeExerciseList : Decoder (List ( String, ExerciseProps ))
decodeExerciseList =
   decodeExercise
   |> keyValuePairs

-- API --

open: Model -> (Model, Cmd Msg)
open model =
  update Open model

getExercise: Int -> Exercises -> Exercise
getExercise id exercises =
  let
    found = exercises
      |> List.filter (\exercise -> exercise.id == id)
      |> List.head
  in
    case found of
      Just val ->
        val
      Nothing ->
        emptyExercise

-- Helpers --

sortById: (String, ExerciseProps) -> (String, ExerciseProps) -> Order
sortById a b =
  case compare (snd a |> .id) (snd b |> .id) of
    GT -> GT
    EQ -> EQ
    LT -> LT

tupleToRecord: (String, ExerciseProps) -> Exercise
tupleToRecord (str, props) =
  { name = str, id = props.id, color = props.color, videoId = props.videoId }

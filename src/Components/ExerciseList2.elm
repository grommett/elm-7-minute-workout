module ExerciseList2 exposing(view)
import Html exposing (Html, div, p, ul, li, button, text, section, h1, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Http
import Json.Decode as Json exposing(..)
import Task


main =
  App.program
  { init = update GetExercises (Model [] defaultExercise "" "")
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

type Msg
  = GetExercises
  | Open
  | Select Exercise
  | FetchSucceed Exercises
  | FetchFail Http.Error

type alias Exercises =
  List (String, ExerciseProps)

type alias Exercise =
  (String, ExerciseProps)

type alias ExerciseProps =
  { color: String
  , id: Int
  , videoId: String
  }

type alias ExerciseRecord =
  { color: String
  , id: Int
  , videoId: String
  , name: String
  }

type alias Model =
  { exercises: Exercises
  , exercise: Exercise
  , viewState: String
  , errorMessage: String
  }

init : (Model, Cmd Msg)
init =
  ( Model [] defaultExercise "Loading" ""
  , Cmd.none
  )

defaultExercise: Exercise
defaultExercise=
  ("", {color="", id= -1, videoId=""})

getExerciseList : Cmd Msg
getExerciseList =
  let
    url =
      "//testing-35f49.firebaseio.com/exercises.json"
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeExerciseList url)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetExercises ->
      {model | viewState = "Loading"} ! [getExerciseList]

    Select exercise ->
      ({model | exercise = exercise}, Cmd.none)

    Open ->
      ({model | viewState="open"}, Cmd.none)

    FetchSucceed exercises ->
      let
        sorted = List.sortWith sortById exercises
        log = Debug.log "converted to records" (convertToRecord sorted)
      in
        ({model | exercises = sorted, viewState="Loaded", exercise = getExercise 0 exercises}, Cmd.none)

    FetchFail error ->
      let
        errorMessage = "Error loading the exercises!"
        log = Debug.log "error" error
      in
        (Model [] defaultExercise "Error" errorMessage, Cmd.none)

sortById a b =
  case compare (snd a |> .id) (snd b |> .id) of
    GT -> GT
    EQ -> EQ
    LT -> LT

view: Model -> Html Msg
view model =
  div[]
  [ text model.viewState
  , ul []
      (List.indexedMap exerciseListItem model.exercises)
  , if model.viewState == "Error" then
      p [] [text model.errorMessage]
    else text ""
  , p [] [text (toString model)]
  ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

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
   --|> convertToRecord

-- convertTupelToRecord: Exercise -> ExerciseRecord
convertTupelToRecord tpl =
  let
    name = fst tpl
    color = snd tpl |> .color
    id = snd tpl |> .id
    videoId = snd tpl |> .videoId
  in
    {name=name, color=color, id=id, videoId=videoId}

-- convertToRecord: List (String, ExerciseProps) -> List ExerciseRecord
convertToRecord exercises =
  List.map (\exercise -> convertTupelToRecord exercise) exercises

exerciseListItem: Int -> Exercise -> Html Msg
exerciseListItem num exercise =
  let
    exeProps = snd exercise
    color = exeProps.color
    name = fst exercise
  in
    li [ onClick (Select exercise), class ("exercise-item " ++ color) ]
      [ span [ class "exercise-item-number"] [ text (toString num) ]
      , text name
      , span [ class "exercise-item-arrow"] [ text "›" ]
      ]

open: Model -> (Model, Cmd Msg)
open model =
  update Open model

getExercise: Int -> Exercises -> Exercise
getExercise id exercises =
  let
    found = exercises
      |> List.filter (\exercise -> (snd exercise |> .id) == id)
      |> List.head
  in
    case found of
      Just val ->
        val
      Nothing ->
        defaultExercise

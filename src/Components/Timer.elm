module Timer exposing (Model, Msg, subscription, update, model, toggle, setCountdown)
import Time exposing (Time, every, second)

type Msg
  = Tick Float
  | Toggle
  | SetCountDown Int

type alias Model =
  { countdown : Int
  , paused : Bool
  , duration : Int
  }

model: Model
model =
  { countdown = 10
  , paused = True
  , duration = 10
  }

update: Msg -> Model -> Model
update msg model =
  case msg of
    Tick _->
      { model
      | countdown = model.countdown - 1
      , paused = (if model.countdown < 0 then True else False)
      }

    Toggle ->
      { model | paused = not model.paused }

    SetCountDown int ->
      { model | countdown = int, duration = int }

toggle: Model -> Model
toggle model =
  update Toggle model

getDuration : Int
getDuration =
  model.duration

setCountdown : Model -> Int -> Model
setCountdown model int =
  update (SetCountDown int) model

subscription: Model -> Sub Msg
subscription model =
  if model.paused then
    Sub.none
  else
    Time.every Time.second Tick

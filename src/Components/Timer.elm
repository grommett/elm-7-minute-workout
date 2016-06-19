module Timer exposing (Model, Msg, subscription, update, model, toggle, setCountdown)
import Time exposing (Time, every, second)

type Msg
  = Tick Float
  | Toggle
  | SetCountDown Int

type alias Model =
  { countdown : Int
  , paused : Bool
  }

model: Model
model =
  { countdown = 10
  , paused = True
  }

update: Msg -> Model -> Model
update msg model =
  case msg of
    Tick _->
      { model
      | countdown = (if model.countdown - 1 < 0 then 0 else model.countdown - 1)
      }

    Toggle ->
      {model | paused = not model.paused}

    SetCountDown int ->
      {model | countdown = int}

toggle: Model -> Model
toggle model =
  update Toggle model

setCountdown: Model -> Int -> Model
setCountdown model int =
  update (SetCountDown int) model

subscription: Model -> Sub Msg
subscription model =
  if model.paused then
    Sub.none
  else
    Time.every Time.second Tick

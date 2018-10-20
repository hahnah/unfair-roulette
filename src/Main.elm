import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List
import Svg exposing (svg, circle)
import Svg.Attributes exposing (viewBox, width, cx, cy, r, fill, fillOpacity, stroke, strokeWidth, strokeDashoffset, strokeDasharray)

main =
  Browser.sandbox
    { init = init 2
    , update = update
    , view = view
    }


-- MODEL

type alias Model =
  { counters: Counters }

init : Int -> Model
init num =
  let
    counters = List.range 1 num
      |> List.map String.fromInt
      |> List.map (\id -> Counter id "" 0)
  in
    Model counters

type alias Counters = List Counter

type alias Counter =
  { id: String
  , name: String
  , count: Int
  }


-- MSG

type Msg
  = Increment Counter
  | Decrement Counter
  | AddItem


-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment counter ->
      let
        (front, back) = separateIntoFrontAndBack model.counters counter
        updatedCounter = { counter | count = counter.count + 1}
        updatedCounters = front ++ [updatedCounter] ++ back
      in
        { model | counters = updatedCounters }

    Decrement counter ->
      if 0 < counter.count then
        let
          (front, back) = separateIntoFrontAndBack model.counters counter
          updatedCounter = { counter | count = counter.count - 1}
          updatedCounters = front ++ [updatedCounter] ++ back
        in
          { model | counters = updatedCounters }
      else
        model

    AddItem ->
      { model | counters = List.append model.counters <| [newCounter model.counters] }

newCounter : Counters -> Counter
newCounter counters =
  let
    newID = String.fromInt <| List.length counters + 1
  in
    Counter newID "" 0 

separateIntoFrontAndBack : Counters -> Counter -> (Counters, Counters)
separateIntoFrontAndBack counters counter =
  let
    (frontCounters, remainedCounters) = List.partition (\x -> x.id < counter.id) counters
    backCounters = Maybe.withDefault [] <| List.tail remainedCounters
  in
    (frontCounters, backCounters)


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewRoulette model.counters
    , div [] <| List.map viewCounter model.counters
    , button [ onClick AddItem ] [ text "Add" ]
    ]

viewRoulette : Counters -> Html Msg
viewRoulette counters =
  svg
    [ viewBox "0 0 63.6619772368 63.6619772368" , width "300px" ]
    [ viewFanShape 0 30 "#ff0000"
    , viewFanShape 30 40 "#00ff00"
    , viewFanShape 70 30 "#0000ff"
    ]

viewFanShape : Float -> Float -> String -> Html Msg
viewFanShape offset percentage color =
  let
    strokeDashoffset_ = String.fromFloat <| 25.0 - offset
    strokeDasharray_ = String.fromFloat percentage ++ " " ++ (String.fromFloat <| 100.0 - percentage)
  in
    circle
      [ cx "31.8309886184", cy "31.8309886184", r "15.9154943092"
      , fill "#ffffff", fillOpacity "0.0"
      , stroke color, strokeWidth "31.8309886184", strokeDashoffset strokeDashoffset_, strokeDasharray strokeDasharray_ ]
      []

viewCounter : Counter -> Html Msg
viewCounter counter =
  let
    count = String.fromInt counter.count
  in
    div []
      [ button [ onClick (Decrement counter) ] [ text "-" ]
      , div [] [ text count ]
      , button [ onClick (Increment counter) ] [ text "+" ]
      ]

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style)
import List
import Maybe
import Svg exposing (svg, circle)
import Svg.Attributes exposing (viewBox, width, cx, cy, r, fill, fillOpacity, stroke, strokeWidth, strokeDashoffset, strokeDasharray, transform)

main =
  Browser.sandbox
    { init = init 2
    , update = update
    , view = view
    }


-- MODEL

type alias Model =
  { counters: Counters
  , maxCounters: Int
  , rotationPercentage: Float
  }

init : Int -> Model
init num =
  let
    counters = List.range 1 num
      |> List.map (\id -> Counter id "" 0)
    maxCounters = List.length colorList
  in
    Model counters maxCounters 0.0

type alias Counters = List Counter

type alias Counter =
  { id: Int 
  , label: String
  , count: Int
  }

type alias FanShape =
  { offset: Float
  , percentage: Float
  , color: String
  }

type alias Color = String

type alias Colors = List Color


-- MSG

type Msg
  = Increment Counter
  | Decrement Counter
  | AddItem
  | DeleteItem Counter
  | ChangeLable Counter String


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
      if List.length model.counters < model.maxCounters then
        { model | counters = List.append model.counters <| [newCounter model.counters] }
      else
        model
    
    DeleteItem counter ->
      let
        (front, back) = separateIntoFrontAndBack model.counters counter
        updatedCounters = front ++ back
      in
        { model | counters = updatedCounters }

    ChangeLable counter newLabel ->
      let
        (front, back) = separateIntoFrontAndBack model.counters counter
        updatedCounter = { counter | label = newLabel}
        updatedCounters = front ++ [updatedCounter] ++ back
      in
        { model | counters = updatedCounters }

newCounter : Counters -> Counter
newCounter counters =
  let
    newID = List.length counters + 1
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
    [ viewRoulette model.counters colorList 25.0
    , div [] <| viewCounters model.counters colorList
    , button [ onClick AddItem ] [ text "Add" ]
    ]

viewRoulette : Counters -> Colors -> Float -> Html Msg
viewRoulette counters colors rotationPercentage =
  let
    counts = List.map (\counter -> toFloat counter.count) counters
    total = List.sum counts
    percentages = List.map (\count -> 100.0 * count / total) counts
    offsets = List.foldl (\percentage acc -> List.append acc [(Maybe.withDefault 0.0 <| List.maximum acc) + percentage]) [0.0] percentages
    fanShapes = List.map3 (\offset percentage color -> FanShape offset percentage color) offsets percentages colors
  in
    svg
      [ viewBox "0 0 63.6619772368 63.6619772368" , width "300px" ]
      (List.map (\fanShape -> viewFanShape fanShape rotationPercentage) fanShapes)
      
viewFanShape : FanShape -> Float -> Html Msg
viewFanShape fanShape rotationPercentage =
  let
    strokeDashoffset_ = String.fromFloat <| 25.0 - fanShape.offset - rotationPercentage
    strokeDasharray_ = String.fromFloat fanShape.percentage ++ " " ++ (String.fromFloat <| 100.0 - fanShape.percentage)
  in
    circle
      [ cx "31.8309886184", cy "31.8309886184", r "15.9154943092"
      , fill "#ffffff", fillOpacity "0.0"
      , stroke fanShape.color, strokeWidth "31.8309886184", strokeDashoffset strokeDashoffset_, strokeDasharray strokeDasharray_ ]
      []

colorList : Colors
colorList =
  [ "#ffec00"
  , "#9cc45a"
  , "#fadce5"
  , "#9b7bb4"
  , "#ecaeca"
  , "#d8e27f"
  , "#c4a16c"
  , "#f7edd0"
  , "#797d8a"
  , "#19a591"
  ]

viewCounters : Counters -> Colors -> List (Html Msg)
viewCounters counters colors =
  List.map2 viewCounter counters colors

viewCounter : Counter -> Color -> Html Msg
viewCounter counter color =
  let
    count = String.fromInt counter.count
  in
    div []
      [ div [ style "display" "inline", style "background-color" color ] [ text "　" ]
      , input [ style "type" "text", onInput <| ChangeLable counter ] [ text counter.label ]
      , button [ style "display" "inline", onClick (Decrement counter) ] [ text "-" ]
      , div [ style "display" "inline" ] [ text count ]
      , button [ style "display" "inline", onClick (Increment counter) ] [ text "+" ]
      , button [ style "display" "inline", onClick (DeleteItem counter) ] [ text "Delete" ]
      ]

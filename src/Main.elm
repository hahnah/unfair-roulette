module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style)
import List
import Tuple
import Maybe exposing (Maybe)
import Svg exposing (svg, circle, polygon)
import Svg.Attributes exposing (viewBox, width, cx, cy, r, fill, points, fillOpacity, stroke, strokeWidth, strokeDashoffset, strokeDasharray, transform)
import Time
import Random

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> init 2
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { scene: Scene
  , counters: Counters
  , maxCounters: Int
  , rotationPercentage: Float
  , rotationPercentageVelocity: Float
  , decayRate: Float
  , pointedCounter: Counter
  }

init : Int -> (Model, Cmd Msg)
init num =
  let
    counters = List.range 1 num
      |> List.map (\id -> Counter id "" 0)
    maxCounters = List.length colorList
  in
    (Model EditingRoulette counters maxCounters 0.0 0.0 0.0 dummyCounter, Cmd.none)

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

type Scene
  = EditingRoulette
  | RouletteSpinning
  | RouletteStopped
  | ResultShowed

type alias RotationRange =
  { min: Float
  , max: Float
  }


-- MSG

type Msg
  = Increment Counter
  | Decrement Counter
  | AddItem
  | DeleteItem Counter
  | ChangeLable Counter String
  | OnClickStart
  | StartSpinningRoulette (Float, Float)
  | SpinRoulette Time.Posix
  | StopRoulette Time.Posix
  | ShowResult Counter Time.Posix
  | HideResult

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model.scene) of
    (Increment counter, EditingRoulette) ->
      let
        (front, back) = separateIntoFrontAndBack model.counters counter
        updatedCounter = { counter | count = counter.count + 1}
        updatedCounters = front ++ [updatedCounter] ++ back
      in
        ({ model | counters = updatedCounters }, Cmd.none)

    (Decrement counter, EditingRoulette) ->
      if 0 < counter.count then
        let
          (front, back) = separateIntoFrontAndBack model.counters counter
          updatedCounter = { counter | count = counter.count - 1}
          updatedCounters = front ++ [updatedCounter] ++ back
        in
          ({ model | counters = updatedCounters }, Cmd.none)
      else
        (model, Cmd.none)

    (AddItem, EditingRoulette) ->
      if List.length model.counters < model.maxCounters then
        ({ model | counters = List.append model.counters <| [newCounter model.counters] }, Cmd.none)
      else
        (model, Cmd.none)
    
    (DeleteItem counter, EditingRoulette) ->
      let
        (front, back) = separateIntoFrontAndBack model.counters counter
        updatedCounters = front ++ back
      in
        ({ model | counters = updatedCounters }, Cmd.none)

    (ChangeLable counter newLabel, EditingRoulette) ->
      let
        (front, back) = separateIntoFrontAndBack model.counters counter
        updatedCounter = { counter | label = newLabel}
        updatedCounters = front ++ [updatedCounter] ++ back
      in
        ({ model | counters = updatedCounters }, Cmd.none)
    
    (OnClickStart, EditingRoulette) ->
      (model, Random.generate StartSpinningRoulette <| Random.pair (Random.float 10.0 20.0) (Random.float 0.97 0.99))

    (StartSpinningRoulette (initialVelocity, decayRate), EditingRoulette) ->
      ({ model | scene = RouletteSpinning, rotationPercentageVelocity = initialVelocity, decayRate = decayRate }, Cmd.none)

    (SpinRoulette _, RouletteSpinning) ->
      let
        (rotationPercentage_, rotationPercentageVelocity_) = updateRotation model.rotationPercentage model.rotationPercentageVelocity model.decayRate
      in
        ({ model | rotationPercentage = rotationPercentage_, rotationPercentageVelocity = rotationPercentageVelocity_}, Cmd.none)
    
    (StopRoulette _, RouletteSpinning) ->
      ({ model | scene = RouletteStopped }, Cmd.none)
    
    (ShowResult pointedItem _, RouletteStopped) ->
      ({ model | scene = ResultShowed, pointedCounter = pointedItem}, Cmd.none)
    
    (HideResult, ResultShowed) ->
      ({ model | scene = EditingRoulette, pointedCounter = dummyCounter }, Cmd.none)
    
    (_, _) ->
      (model, Cmd.none)

dummyCounter : Counter
dummyCounter =
  Counter -1 "" 0

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

updateRotation : Float -> Float -> Float -> (Float, Float)
updateRotation rotationPercentage rotationPercentageVelocity decayRate =
  let
    tempRotationPercentage = rotationPercentage + rotationPercentageVelocity
    newRotationPercentage = 
      if tempRotationPercentage >= 100.0 then
        tempRotationPercentage - 100.0
      else
        tempRotationPercentage
    tempVelocity = decayRate * rotationPercentageVelocity
    newRotationPercentageVelocity =
      if tempVelocity > 0.02 then
        tempVelocity
      else
        0.0
  in
    (newRotationPercentage, newRotationPercentageVelocity)

checkPointedItem : Counters -> Float -> Counter
checkPointedItem counters rotationPercentage =
  let
    maybePointedItem =
      calculateCollisionRanges counters rotationPercentage
        |> zip counters
        |> List.sortBy (\(_, rotationRange) -> rotationRange.max)
        |> List.head
        |> Maybe.map (\(counter, rotationRange) -> counter)
  in
    case maybePointedItem of
      Just item ->
        item 
    
      Nothing ->
        dummyCounter

calculateCollisionRanges : Counters -> Float -> List RotationRange
calculateCollisionRanges counters rotationPercentage =
  let
    counts = List.map (\counter -> toFloat counter.count) counters
    total = List.sum counts
    percentages = List.map (\count -> 100.0 * count / total) counts
    offsets = List.foldl (\percentage acc -> List.append acc [(Maybe.withDefault 0.0 <| List.maximum acc) + percentage]) [0.0] percentages
  in
    List.map2 (\percentage offset ->
      RotationRange (25 - offset - percentage) <| 25.0 - offset) percentages offsets
  
zip : List a -> List b -> List (a, b)
zip xs ys =
  List.map2 Tuple.pair xs ys


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewRoulette model.counters colorList model.rotationPercentage
    , div [] <| viewCounters model.counters colorList
    , button [ onClick AddItem ] [ text "Add" ]
    , button [ onClick OnClickStart ] [ text "Start" ]
    , viewResult model.scene model.pointedCounter
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
      (List.append (List.map (\fanShape -> viewFanShape fanShape rotationPercentage) fanShapes) [viewRoulettePointer])
      
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

viewRoulettePointer : Html Msg
viewRoulettePointer =
  polygon
    [ points "29.5309886184,0 34.3309886184,0 31.8309886184,6"
    , style "fill" "#e50011"
    ]
    []

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

viewResult : Scene -> Counter -> Html Msg
viewResult scene result =
  case scene of
    ResultShowed ->
      div []
        [ text result.label
        , button [ onClick HideResult ] [ text "Close"] ]
    _ ->
      text ""

-- Subscription

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.scene of
    RouletteSpinning ->
      if model.rotationPercentageVelocity /= 0 then
        Time.every 30 SpinRoulette
      else
        Time.every 200 StopRoulette
    RouletteStopped ->
      Time.every 200 <| ShowResult (checkPointedItem model.counters model.rotationPercentage)
    _ ->
      Sub.none
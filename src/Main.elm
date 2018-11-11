module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input, h2, h4, node)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, class, value, placeholder, href, rel)
import List
import Tuple
import Maybe exposing (Maybe)
import Svg exposing (svg, circle, polygon)
import Svg.Attributes exposing (viewBox, width, cx, cy, r, fill, points, fillOpacity, stroke, strokeWidth, strokeDashoffset, strokeDasharray, transform)
import Time
import Random
import Dialog
import Process
import Task

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> init muximumCounters
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
  , goalRotation: Float
  , pointedCounter: Counter
  , cheatedGoalRange: Maybe Range
  }

init : Int -> (Model, Cmd Msg)
init num =
  let
    counters = List.range 1 num
      |> List.map (\id -> Counter id "" 0)
    maxCounters = List.length colorList
    cheatedGoalRange = Nothing
  in
    (Model EditingRoulette counters maxCounters 0.0 0.0 initialDecayRate 0.0 dummyCounter cheatedGoalRange, Cmd.none)

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
  | RouletteSpinningTowardsStop
  | RouletteStopped
  | ResultShowed

type alias Range =
  { min: Float
  , max: Float
  }

type alias RotationRange =
 { min: Float
 , max: Float
 }

muximumCounters : Int
muximumCounters = 12

initialDecayRate : Float
initialDecayRate = 0.99

smootherDecayRate : Float
smootherDecayRate = 0.9999

initialVelocityRange : Range
initialVelocityRange = Range 10.0 20.0

limitVelocityOfUniformAcceleration : Float
limitVelocityOfUniformAcceleration = 0.2

minimumVelocity : Float
minimumVelocity = 0.02

fairGoalRange : Range
fairGoalRange = Range 0.0 100.0

milliSecondsToKeepRouletteStopUntilResult : Float
milliSecondsToKeepRouletteStopUntilResult = 900.0

dummyCounter : Counter
dummyCounter = Counter -1 "" 0

colorList : Colors
colorList =
  [ "#ff7f7f"
  , "#bf7fff"
  , "#bfff7f"
  , "#7fffff"
  , "#ff7fbf"
  , "#7f7fff"
  , "#ffff7f"
  , "#ff7fff"
  , "#7fbfff"
  , "#7fff7f"
  , "#ffbf7f"
  , "#7fbfff"
  ]

roulettePointerColor : Color
roulettePointerColor = "#e50011"


-- MSG

type Msg
  = Increment Counter
  | Decrement Counter
  | ChangeLable Counter String
  | ChangeCount Counter String
  | Clear Counter
  | Cheat Counter
  | OnClickStart
  | StartSpinningRoulette Float (Float, Float)
  | SpinRoulette Time.Posix
  | AdjustDecayRateForSmoothStop Float
  | StopRoulette Float
  | ShowResult
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

    (ChangeLable counter newLabel, EditingRoulette) ->
      let
        (front, back) = separateIntoFrontAndBack model.counters counter
        updatedCounter = { counter | label = newLabel}
        updatedCounters = front ++ [updatedCounter] ++ back
      in
        ({ model | counters = updatedCounters }, Cmd.none)
    
    (ChangeCount counter newCount, EditingRoulette) ->
      let
        (front, back) = separateIntoFrontAndBack model.counters counter
        updatedCounter = { counter | count = String.toInt newCount |> Maybe.withDefault (if newCount == "" then 0 else counter.count) }
        updatedCounters = front ++ [updatedCounter] ++ back
      in
        ({ model | counters = updatedCounters }, Cmd.none)
    
    (Clear counter, EditingRoulette) ->
      let
        (front, back) = separateIntoFrontAndBack model.counters counter
        updatedCounter = { counter | count = 0, label = "" }
        updatedCounters = front ++ [updatedCounter] ++ back
      in
        ({ model | counters = updatedCounters }, Cmd.none)
    
    (Cheat counter, EditingRoulette) ->
      let
        cheatedGoalRange_ =
          if counter.count > 0 then
            calculateCollisionRanges model.counters 0.0
              |> List.take counter.id
              |> List.reverse
              |> List.head
              |> Maybe.andThen (\maybeRange -> Just <| Range (100.0 - maybeRange.max) (100.0 - maybeRange.min))
          else
            Nothing
      in
        ({model | cheatedGoalRange = cheatedGoalRange_}, Cmd.none)
    
    (OnClickStart, EditingRoulette) ->
      let
        goalRange =
          Maybe.withDefault fairGoalRange model.cheatedGoalRange
      in
        (model, Random.generate (StartSpinningRoulette initialDecayRate) <| Random.pair (Random.float initialVelocityRange.min initialVelocityRange.max) (Random.float goalRange.min goalRange.max))

    (StartSpinningRoulette decayRate_ (initialVelocity, goal), EditingRoulette) ->
      if isThereEnogthCountersToStart model.counters then
        ({ model | scene = RouletteSpinning, decayRate = decayRate_,rotationPercentageVelocity = initialVelocity, goalRotation = goal }, Cmd.none)
      else
        (model, Cmd.none)

    (SpinRoulette _, RouletteSpinning) ->
      let
        pointedCounter_ =
          calculateCollisionRanges model.counters model.rotationPercentage
            |> List.map2 (\counter collisionRange -> Tuple.pair counter <| willBeNewlyPointed model.rotationPercentageVelocity collisionRange) model.counters
            |> List.filter (\(counter, willBeNewlyPointed_) -> willBeNewlyPointed_)
            |> List.head
            |> Maybe.withDefault (model.pointedCounter, False)
            |> Tuple.first
        (rotationPercentage_, rotationPercentageVelocity_) = updateRotation model.rotationPercentage model.rotationPercentageVelocity model.decayRate
        model_ =
          { model | rotationPercentage = rotationPercentage_, rotationPercentageVelocity = rotationPercentageVelocity_, pointedCounter = pointedCounter_ }
      in
        if model.rotationPercentageVelocity > limitVelocityOfUniformAcceleration then
          (model_, Cmd.none)
        else
          update (AdjustDecayRateForSmoothStop smootherDecayRate) model_
    
    (SpinRoulette _, RouletteSpinningTowardsStop) ->
      let
        pointedCounter_ =
          calculateCollisionRanges model.counters model.rotationPercentage
            |> List.map2 (\counter collisionRange -> Tuple.pair counter <| willBeNewlyPointed model.rotationPercentageVelocity collisionRange) model.counters
            |> List.filter (\(counter, willBeNewlyPointed_) -> willBeNewlyPointed_)
            |> List.head
            |> Maybe.withDefault (model.pointedCounter, False)
            |> Tuple.first
        (rotationPercentage_, rotationPercentageVelocity_) = updateRotation model.rotationPercentage model.rotationPercentageVelocity model.decayRate
      in
        if willReachGoal model.goalRotation model.rotationPercentage model.rotationPercentageVelocity then
          update (StopRoulette milliSecondsToKeepRouletteStopUntilResult) model
        else
          ({ model | rotationPercentage = rotationPercentage_, rotationPercentageVelocity = rotationPercentageVelocity_, pointedCounter = pointedCounter_}, Cmd.none)
    
    (AdjustDecayRateForSmoothStop decayRate_, RouletteSpinning) ->
      ({ model | decayRate = decayRate_, scene = RouletteSpinningTowardsStop }, Cmd.none)
    
    (StopRoulette forMilliSeconds, RouletteSpinningTowardsStop) ->
      ({ model | scene = RouletteStopped}, Process.sleep forMilliSeconds |> Task.perform (always ShowResult))

    (ShowResult, RouletteStopped) ->
      ({ model | scene = ResultShowed }, Cmd.none)
    
    (HideResult, ResultShowed) ->
      ({ model | scene = EditingRoulette, pointedCounter = dummyCounter }, Cmd.none)
    
    (_, _) ->
      (model, Cmd.none)

isZeroRange : Range -> Bool
isZeroRange range =
  if range.min == range.max then
    True
  else
    False

isThereEnogthCountersToStart : Counters -> Bool
isThereEnogthCountersToStart counters =
  counters
    |> List.filter (\counter -> counter.count > 0)
    |> List.length
    |> (<) 1

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
    newRotationPercentage =
      rotationPercentage + rotationPercentageVelocity
        |> carryDownUnder 100.0 100.0
    tempVelocity =
      decayRate * rotationPercentageVelocity
    newRotationPercentageVelocity =
      if tempVelocity > minimumVelocity then
        tempVelocity
      else
        0.0
  in
    (newRotationPercentage, newRotationPercentageVelocity)

willBeNewlyPointed : Float -> RotationRange -> Bool
willBeNewlyPointed rotationVelocity collisionRange  =
  if carryDownUnder 200.0 100.0 (collisionRange.max + rotationVelocity) > 100.0
    && carryDownUnder 200.0 100.0 (collisionRange.min + rotationVelocity) <= 100
  then
    True
  else
    False

willReachGoal : Float -> Float -> Float -> Bool
willReachGoal goal rotation velocity =
  if rotation < goal && goal <= rotation + velocity then
    True
  else
    False

carryDownUnder : Float -> Float -> Float -> Float
carryDownUnder maximum decrementStep value =
  if value < maximum then
    value
  else
    carryDownUnder value decrementStep <| value - decrementStep 

calculateCollisionRanges : Counters -> Float -> List RotationRange
calculateCollisionRanges counters rotationPercentage =
  let
    counts = List.map (\counter -> toFloat counter.count) counters
    total = List.sum counts
    percentages = List.map (\count -> 100.0 * count / total) counts
    offsets =
      List.foldl (\percentage acc -> List.append acc [(Maybe.withDefault 0.0 <| List.maximum acc) + percentage]) [0.0] percentages
        |> List.map ((+) rotationPercentage)
  in
    List.map2 (\percentage offset -> RotationRange offset <| offset + percentage) percentages offsets


-- VIEW

view : Model -> Html Msg
view model =
  div [ style "text-align" "center", style "margin" "0 auto", style "padding-top" "1em", style "display" "block" ]
    [ bootstrap
    , div [ style "width" "500px", style "display" "inline-block" ]
      [ viewRoulette model.counters colorList model.rotationPercentage
      , viewStartButton model.scene
      , viewCurrentlyPointedLable model.scene model.pointedCounter
      ] 
    , div [ style "width" "500px", style "display" "inline-block" ]
      [ div [] <| viewCounters model.counters colorList
      , viewResultDialog model.scene model.pointedCounter
      ]
    ]

viewRoulette : Counters -> Colors -> Float -> Html Msg
viewRoulette counters colors rotationPercentage =
  let
    counts = List.map (\counter -> toFloat counter.count) counters
    total = List.sum counts
    percentages = List.map (\count -> 100.0 * count / total) counts
    offsets =
      List.foldl (\percentage acc -> List.append acc [(Maybe.withDefault 0.0 <| List.maximum acc) + percentage]) [0.0] percentages
        |> List.map ((+) rotationPercentage)
    fanShapes = List.map3 (\offset percentage color -> FanShape offset percentage color) offsets percentages colors
  in
    svg
      [ viewBox "0 0 63.6619772368 63.6619772368", width "500px", style "transform" "rotate(90deg)" ]
      (List.append (List.map (\fanShape -> viewFanShape fanShape) fanShapes) [viewRoulettePointer])
      
viewFanShape : FanShape -> Html Msg
viewFanShape fanShape =
  let
    strokeDashoffset_ = String.fromFloat <|  -fanShape.offset -- Fan shape direction is opposite to Svg.circle, becouse of the specification of dassArray. So I negat fanshape.offset.
    strokeDasharray_ = String.fromFloat fanShape.percentage ++ " " ++ (String.fromFloat <| 100.0 - fanShape.percentage)
  in
    circle
      [ cx "31.8309886184", cy "31.8309886184", r "15.9154943092"
      , fill "#ffffff", fillOpacity "0.0"
      , stroke fanShape.color, strokeWidth "31.8309886184", strokeDashoffset strokeDashoffset_, strokeDasharray strokeDasharray_ ]
      []

viewRoulettePointer : Html Msg
viewRoulettePointer =
  polygon
    [ points "63.6619772368,29.5309886184 63.6619772368,34.3309886184 57.6619772368,31.8309886184"
    , style "fill" roulettePointerColor
    ]
    []

viewCounters : Counters -> Colors -> List (Html Msg)
viewCounters counters colors =
  List.map2 viewCounter counters colors

viewCounter : Counter -> Color -> Html Msg
viewCounter counter color =
  let
    count = String.fromInt counter.count
    placeholder_ = "No." ++ String.fromInt counter.id
  in
    div []
      [ div [ class "input-group mb-3", style "display" "inline", style "margin-right" "0.2em" ]
          [ div [ class "input-group-prepend", style "display" "inline" ]
            [ Html.span [ style "display" "inline", style "padding" "0.35em", style "background-color" color, style "border-color" color, style "cursor" "default", onClick (Cheat counter) ] [ text "　" ]
            , input [ style "type" "text", style "display" "inline", style "width" "12em", value counter.label, placeholder placeholder_, onInput <| ChangeLable counter ] [ text counter.label ]
            ]
          ]
      , input [ style "type" "number", style "width" "2.5em", value count, placeholder placeholder_, onInput <| ChangeCount counter, style "display" "inline" ] [ text count ]
      , button [ class "btn btn-outline-secondary", style "display" "inline", onClick (Increment counter) ] [ text "+" ]
      , button [ class "btn btn-outline-secondary", style "display" "inline", onClick (Clear counter) ] [ text "Clear" ]
      ]

viewStartButton : Scene -> Html Msg
viewStartButton scene =
  case scene of
    EditingRoulette ->
      div [] [ button [ class "btn btn-outline-primary", style "width" "400px", style "margin" " 0.5em 0 2em 0", onClick OnClickStart ] [ text "Start" ] ]
  
    _ ->
      text ""
          

viewCurrentlyPointedLable : Scene -> Counter -> Html Msg
viewCurrentlyPointedLable scene pointedCounter =
  let
    resultText =
      if pointedCounter.label == "" then
        "No." ++ String.fromInt pointedCounter.id
      else
        pointedCounter.label
  in
    case scene of
      RouletteSpinning ->
        div [ style "height" "1.3em", style "margin" "0.6em 0 1em 0" ] [ text resultText ]
      
      RouletteSpinningTowardsStop ->
        div [ style "height" "1.3em", style "margin" "0.6em 0 1em 0" ] [ text resultText ]

      RouletteStopped ->
        div [ style "height" "1.3em", style "margin" "0.6em 0 1em 0" ] [ text resultText ]
      
      ResultShowed ->
        div [ style "height" "1.3em", style "margin" "0.6em 0 1em 0" ] [ text resultText ]
  
      _ ->
        text ""

viewResultDialog : Scene -> Counter -> Html Msg
viewResultDialog scene pointedCounter =
  Dialog.view <|
    case scene of
      ResultShowed ->
        Just (dialogConfig pointedCounter)

      _ ->
        Nothing
    
dialogConfig : Counter -> Dialog.Config Msg
dialogConfig pointedCounter =
  let
    resultLabel =
      if pointedCounter.label == "" then
        "No." ++ String.fromInt pointedCounter.id
      else
        pointedCounter.label
  in
    { closeMessage = Just HideResult
    , containerClass = Nothing
    , header = Just <| h4 [] [ text "RESULT" ]
    , body = Just <| h2 [] [ text resultLabel ]
    , footer = Just <| button [ class "btn btn-success", onClick HideResult ] [ text "OK" ]
    }

bootstrap : Html msg
bootstrap =
    node "link"
        [ href "https://maxcdn.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css"
        , rel "stylesheet"
        ]
        []

-- Subscription

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.scene of
    RouletteSpinning ->
      Time.every 30 SpinRoulette
    RouletteSpinningTowardsStop ->
      Time.every 30 SpinRoulette
    _ ->
      Sub.none
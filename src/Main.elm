module Main exposing (main)


import Bank exposing (Bank, KeyConfig, Kit)
import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import Key exposing (Key)
import Process
import Task


main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Flags =
  JE.Value


type alias Model =
  Maybe State


type alias State =
  { bank : Bank
  , isOn : Bool
  , text : String
  , volume : Int
  }


init : Flags -> (Model, Cmd msg)
init value =
  let
    model =
      case JD.decodeValue Bank.decoder value of
        Ok bank ->
          Just
            { bank = bank
            , isOn = False
            , text = ""
            , volume = 50
            }

        Err _ ->
          Nothing
  in
  ( model
  , Cmd.none
  )


-- UPDATE


type Msg
  = ToggledPower Bool
  | ToggledBank Bool
  | ChangedVolume String
  | DisplayTimeUp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Just state ->
      updateState msg state
        |> Tuple.mapFirst Just

    Nothing ->
      ( model
      , Cmd.none
      )


updateState : Msg -> State -> (State, Cmd Msg)
updateState msg state =
  case msg of
    ToggledPower isOn ->
      let
        text =
          if isOn then
            "On"
          else
            "Off"
      in
      { state | isOn = isOn }
        |> setDisplay text

    ToggledBank isBank2 ->
      let
        bank =
          if Bank.isBank2 state.bank == isBank2 then
            state.bank
          else
            Bank.switch state.bank

        kit =
          Bank.kit bank
      in
      { state | bank = bank }
        |> setDisplay kit.name

    ChangedVolume volumeAsString ->
      case String.toInt volumeAsString of
        Just volumeAsInt ->
          let
            volume =
              clamp 0 100 volumeAsInt
          in
          { state | volume = volume }
            |> setDisplay ("Volume " ++ String.fromInt volume)

        Nothing ->
          ( state
          , Cmd.none
          )


    DisplayTimeUp ->
      ( { state | text = "" }
      , Cmd.none
      )


setDisplay : String -> State -> (State, Cmd Msg)
setDisplay text state =
  ( { state | text = text }
  , delay 500 DisplayTimeUp
  )


delay : Float -> msg -> Cmd msg
delay time onExpired =
  Process.sleep time
    |> Task.perform (always onExpired)


-- VIEW


view : Model -> H.Html Msg
view model =
  case model of
    Just { bank, isOn, text, volume } ->
      viewLayout <| viewDrumMachine bank isOn text volume

    Nothing ->
      viewError "Sorry, we're unable to start the application since it's not properly configured."


viewLayout : H.Html msg -> H.Html msg
viewLayout html =
  H.div [ HA.class "layout" ]
    [ H.div [ HA.class "layout__wrapper" ]
        [ html ]
    ]


viewDrumMachine : Bank -> Bool -> String -> Int -> H.Html Msg
viewDrumMachine bank isOn text volume =
  let
    isDisabled =
      not isOn
  in
  H.div [ HA.class "drum-machine" ]
    [ H.div [ HA.class "drum-machine__panel" ]
        [ viewPanel isDisabled <| Bank.kit bank ]
    , H.div [ HA.class "drum-machine__controls" ]
        [ H.div [ HA.class "drum-machine__power" ]
            [ viewPower isOn ]
        , H.div [ HA.class "drum-machine__display" ]
            [ viewDisplay text ]
        , H.div [ HA.class "drum-machine__volume" ]
            [ viewVolume isDisabled volume ]
        , H.div [ HA.class "drum-machine__bank" ]
            [ viewBank isDisabled <| Bank.isBank2 bank ]
        ]
    ]


viewPanel : Bool -> Kit -> H.Html msg
viewPanel isDisabled kit =
  H.div [ HA.class "panel" ]
    [ H.div [ HA.class "panel__container" ] <|
        List.indexedMap (viewPanelSpot isDisabled) kit.keyConfigs
    ]


viewPanelSpot : Bool -> Int -> KeyConfig -> H.Html msg
viewPanelSpot isDisabled index config =
  let
    (r, c) =
      (index // 3 + 1, modBy 3 index + 1)
  in
  H.div
    [ HA.class "panel__spot"
    , HA.class <| "r" ++ String.fromInt r
    , HA.class <| "c" ++ String.fromInt c
    ]
    [ viewKey isDisabled config.key ]


viewKey : Bool -> Key -> H.Html msg
viewKey isDisabled key =
  H.button
    [ HA.class "key"
    , HA.disabled isDisabled
    ]
    [ H.text <| Key.toString key ]


viewPower : Bool -> H.Html Msg
viewPower isOn =
  viewLabelledSwitch False ToggledPower isOn "Power"


viewBank : Bool -> Bool -> H.Html Msg
viewBank isDisabled isOn =
  viewLabelledSwitch isDisabled ToggledBank isOn "Bank"


viewLabelledSwitch : Bool -> (Bool -> msg) -> Bool -> String -> H.Html msg
viewLabelledSwitch isDisabled onToggle isOn title =
  H.label [ HA.class "labelled-switch" ]
    [ H.span [ HA.class "labelled-switch__title" ] [ H.text title ]
    , viewSwitch isDisabled onToggle isOn
    ]


viewSwitch : Bool -> (Bool -> msg) -> Bool -> H.Html msg
viewSwitch isDisabled onToggle isOn =
  H.input
    [ HA.type_ "checkbox"
    , HA.class "switch"
    , HA.disabled isDisabled
    , HA.checked isOn
    , HE.onCheck onToggle
    ]
    []


viewDisplay : String -> H.Html msg
viewDisplay text =
  H.div [ HA.class "display" ]
    [ H.text text ]


viewVolume : Bool -> Int -> H.Html Msg
viewVolume =
  viewSlider 0 100 1 ChangedVolume


viewSlider : Int -> Int -> Int -> (String -> msg) -> Bool -> Int -> H.Html msg
viewSlider min max step onChange isDisabled value =
  H.input
    [ HA.type_ "range"
    , HA.min <| String.fromInt min
    , HA.max <| String.fromInt max
    , HA.step <| String.fromInt step
    , HA.class "slider"
    , HA.disabled isDisabled
    , HA.value <| String.fromInt value
    , HE.onInput onChange
    ]
    []


viewError : String -> H.Html msg
viewError text =
  H.div
    [ HA.class "error" ]
    [ H.text text ]

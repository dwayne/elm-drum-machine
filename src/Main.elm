module Main exposing (main)


import Bank exposing (Bank, KeyConfig, Kit)
import Browser
import Html as H
import Html.Attributes as HA
import Json.Decode as JD
import Json.Encode as JE
import Key exposing (Key)


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
  }


init : Flags -> (Model, Cmd msg)
init value =
  let
    model =
      case JD.decodeValue Bank.decoder value of
        Ok bank ->
          Just { bank = bank }

        Err _ ->
          Nothing
  in
  ( model
  , Cmd.none
  )


-- UPDATE


type Msg
  = NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      ( model
      , Cmd.none
      )


-- VIEW


view : Model -> H.Html msg
view model =
  case model of
    Just { bank } ->
      viewDrumMachine bank True "Volume 20" 20

    Nothing ->
      viewError "Sorry, we're unable to start the application since it's not properly configured."


viewDrumMachine : Bank -> Bool -> String -> Int -> H.Html msg
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


viewPower : Bool -> H.Html msg
viewPower isOn =
  viewLabelledSwitch False isOn "Power"


viewBank : Bool -> Bool -> H.Html msg
viewBank isDisabled isOn =
  viewLabelledSwitch isDisabled isOn "Bank"


viewLabelledSwitch : Bool -> Bool -> String -> H.Html msg
viewLabelledSwitch isDisabled isOn title =
  H.label [ HA.class "labelled-switch" ]
    [ H.span [ HA.class "labelled-switch__title" ] [ H.text title ]
    , viewSwitch isDisabled isOn
    ]


viewSwitch : Bool -> Bool -> H.Html msg
viewSwitch isDisabled isOn =
  H.input
    [ HA.type_ "checkbox"
    , HA.class "switch"
    , HA.disabled isDisabled
    , HA.checked isOn
    ]
    []


viewDisplay : String -> H.Html msg
viewDisplay text =
  H.div [ HA.class "display" ]
    [ H.text text ]


viewVolume : Bool -> Int -> H.Html msg
viewVolume =
  viewSlider 0 100 1


viewSlider : Int -> Int -> Int -> Bool -> Int -> H.Html msg
viewSlider min max step isDisabled value =
  H.input
    [ HA.type_ "range"
    , HA.min <| String.fromInt min
    , HA.max <| String.fromInt max
    , HA.step <| String.fromInt step
    , HA.class "slider"
    , HA.disabled isDisabled
    , HA.value <| String.fromInt value
    ]
    []


viewError : String -> H.Html msg
viewError text =
  H.div
    [ HA.class "error" ]
    [ H.text text ]

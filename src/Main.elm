port module Main exposing (main)


import Browser
import Browser.Events
import Html exposing (Html, audio, button, div, h3, input, text)
import Html.Attributes as A exposing (class)
import Html.Events as E
import Json.Encode as Encode
import Json.Decode as Decode
import Process
import Task


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { power : Bool
  , bank : Bool
  , display : String
  , volume : Int
  , activeKey : Maybe Char
  }


type alias DrumPad =
  { name : String
  , id : String
  , keyCode : Int
  , keyTrigger : Char
  }


init : () -> (Model, Cmd msg)
init =
  always
    ( { power = True
      , bank = False
      , display = ""
      , volume = 30
      , activeKey = Nothing
      }
    , Cmd.none
    )


-- UPDATE


type Msg
  = Clicked DrumPad
  | KeyDown Char
  | KeyUp Char
  | ToggledPower
  | ToggledBank
  | ChangedVolume String
  | VolumeAlarm Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clicked { name, id } ->
      ( { model | display = name }
      , play id model.volume
      )

    KeyDown key ->
      ( { model | activeKey = Just key }
      , Cmd.none
      )

    KeyUp key ->
      case findDrumPad key (selectKit model.bank) of
        Just { name, id } ->
          ( { model | display = name, activeKey = Nothing }
          , play id model.volume
          )

        Nothing ->
          ( model
          , Cmd.none
          )

    ToggledPower ->
      ( { model | power = not model.power, display = "" }
      , Cmd.none
      )

    ToggledBank ->
      let
        newBank =
          not model.bank
      in
        ( { model | bank = newBank, display = kitName newBank }
        , Cmd.none
        )

    ChangedVolume newVolumeAsString ->
      case String.toInt newVolumeAsString of
        Just newVolume ->
          ( { model
            | volume = newVolume
            , display = "Volume " ++ newVolumeAsString
            }
          , alarm 500 (VolumeAlarm newVolume)
          )

        Nothing ->
          ( model
          , Cmd.none
          )

    VolumeAlarm volume ->
      ( let
          clearDisplay =
            volume == model.volume && String.startsWith "Volume" model.display
        in
          if clearDisplay then
            { model | display = "" }
          else
            model
      , Cmd.none
      )


play : String -> Int -> Cmd msg
play id volume =
  let
    args =
      Encode.object
        [ ("id", Encode.string id)
        , ("volume", Encode.float (toFloat volume / 100))
        ]
  in
    playAudio args


-- COMMANDS


port playAudio : Encode.Value -> Cmd msg


alarm : Float -> msg -> Cmd msg
alarm time msg =
  Process.sleep time
    |> Task.perform (always msg)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { power } =
  if power then
    Sub.batch
      [ Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
      , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
      ]
  else
    Sub.none


-- DECODERS


keyDecoder : Decode.Decoder Char
keyDecoder =
  Decode.field "key" Decode.string
    |> Decode.andThen
      (\key ->
        case String.uncons key of
          Just (c, "") ->
            Decode.succeed (Char.toUpper c)

          _ ->
            Decode.fail ("Ignored: " ++ key)
      )


-- VIEW


view : Model -> Html Msg
view { power, bank, display, volume, activeKey } =
  div [ class "drum-machine" ]
    [ div [ class "flex" ]
        [ div [ class "mr-40" ]
            [ viewDrumPads power activeKey (selectKit bank) ]
        , div [ class "controls" ]
            [ viewSwitch True "Power" power ToggledPower
            , viewDisplay display
            , viewVolume power volume
            , viewSwitch power "Bank" bank ToggledBank
            ]
        ]
    ]


viewDrumPads : Bool -> Maybe Char -> List DrumPad -> Html Msg
viewDrumPads isEnabled activeKey drumPads =
  div [ class "drum-pads" ]
    [ div [ class "drum-pads__container" ]
        (List.indexedMap (viewDrumPad isEnabled activeKey) drumPads)
    ]


viewDrumPad : Bool -> Maybe Char -> Int -> DrumPad -> Html Msg
viewDrumPad isEnabled activeKey index drumPad =
  let
    (row, col) =
      toPosition index

    rowClass =
      "r" ++ (String.fromInt row)

    colClass =
      "c" ++ (String.fromInt col)

    src =
      "assets/audio/" ++ drumPad.id ++ ".mp3"
  in
    div
      [ class "drum-pad"
      , class rowClass
      , class colClass
      ]
      [ audio [ A.id drumPad.id, A.src src ] []
      , button
          [ class "drum-pad__button"
          , A.classList [ ("is-active", activeKey == Just drumPad.keyTrigger) ]
          , A.disabled (not isEnabled)
          , E.onClick (Clicked drumPad)
          ]
          [ text (String.fromChar drumPad.keyTrigger)
          ]
      ]


viewSwitch : Bool -> String -> Bool -> msg -> Html msg
viewSwitch isEnabled title isOn msg =
  let
    stateClass =
      if isOn then
        "is-on"
      else
        "is-off"

    defaultAttrs =
      [ class ("switch " ++ stateClass) ]

    attrs =
      if isEnabled then
        List.append defaultAttrs [ E.onClick msg ]
      else
        defaultAttrs
  in
    div attrs
      [ div [ class "switch__container" ]
          [ h3 [ class "switch__title" ] [ text title ]
          , div [ class "switch__button-container" ]
              [ div [ class "switch__button" ] [] ]
          ]
      ]


viewDisplay : String -> Html msg
viewDisplay value =
  div [ class "display" ]
    [ if String.isEmpty value then
        text (String.fromChar nonBreakingSpace)
      else
        text value
    ]


viewVolume : Bool -> Int -> Html Msg
viewVolume isEnabled level =
  div [ class "slider" ]
    [ input
        [ A.max "100"
        , A.min "0"
        , A.step "1"
        , A.type_ "range"
        , A.value (String.fromInt level)
        , A.disabled (not isEnabled)
        , E.onInput ChangedVolume
        ]
        []
    ]


-- HELPERS


nonBreakingSpace : Char
nonBreakingSpace = '\u{00A0}'


findDrumPad : Char -> List DrumPad -> Maybe DrumPad
findDrumPad key drumPads =
  case drumPads of
    [] ->
      Nothing

    (drumPad::rest) ->
      if key == drumPad.keyTrigger then
        Just drumPad
      else
        findDrumPad key rest


selectKit : Bool -> List DrumPad
selectKit isSmoothPiano =
  if isSmoothPiano then
    smoothPianoKit
  else
    heaterKit


kitName : Bool -> String
kitName isSmoothPiano =
  if isSmoothPiano then
    "Smooth Piano Kit"
  else
    "Heater Kit"


toPosition : Int -> (Int, Int)
toPosition index =
  (index // 3 + 1, modBy 3 index + 1)


-- DATA


heaterKit : List DrumPad
heaterKit =
  [ { name = "Heater 1"
    , id = "heater-1"
    , keyCode = 81
    , keyTrigger = 'Q'
    }
  , { name = "Heater 2"
    , id = "heater-2"
    , keyCode = 87
    , keyTrigger = 'W'
    }
  , { name = "Heater 3"
    , id = "heater-3"
    , keyCode = 69
    , keyTrigger = 'E'
    }
  , { name = "Heater 4"
    , id = "heater-4"
    , keyCode = 65
    , keyTrigger = 'A'
    }
  , { name = "Clap"
    , id = "clap"
    , keyCode = 83
    , keyTrigger = 'S'
    }
  , { name = "Open HH"
    , id = "open-hh-1"
    , keyCode = 68
    , keyTrigger = 'D'
    }
  , { name = "Kick n' Hat"
    , id = "kick-n-hat"
    , keyCode = 90
    , keyTrigger = 'Z'
    }
  , { name = "Kick"
    , id = "kick"
    , keyCode = 88
    , keyTrigger = 'X'
    }
  , { name = "Closed HH"
    , id = "closed-hh-1"
    , keyCode = 67
    , keyTrigger = 'C'
    }
  ]


smoothPianoKit : List DrumPad
smoothPianoKit =
  [ { name = "Chord 1"
    , id = "chord-1"
    , keyCode = 81
    , keyTrigger = 'Q'
    }
  , { name = "Chord 2"
    , id = "chord-2"
    , keyCode = 87
    , keyTrigger = 'W'
    }
  , { name = "Chord 3"
    , id = "chord-3"
    , keyCode = 69
    , keyTrigger = 'E'
    }
  , { name = "Shaker"
    , id = "shaker"
    , keyCode = 65
    , keyTrigger = 'A'
    }
  , { name = "Open HH"
    , id = "open-hh-2"
    , keyCode = 83
    , keyTrigger = 'S'
    }
  , { name = "Closed HH"
    , id = "closed-hh-2"
    , keyCode = 68
    , keyTrigger = 'D'
    }
  , { name = "Punchy Kick"
    , id = "punchy-kick"
    , keyCode = 90
    , keyTrigger = 'Z'
    }
  , { name = "Side Stick"
    , id = "side-stick"
    , keyCode = 88
    , keyTrigger = 'X'
    }
  , { name = "Snare"
    , id = "snare"
    , keyCode = 67
    , keyTrigger = 'C'
    }
  ]

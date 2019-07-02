port module Main exposing (main)


import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, audio, button, div, h3, input, label, text)
import Html.Attributes as A
import Html.Events as E
import Json.Encode as Encode
import Json.Decode as Decode


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
  , volume : Int
  , display : String
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
      , bank = True
      , volume = 30
      , display = ""
      }
    , Cmd.none
    )


-- UPDATE


type Msg
  = Clicked DrumPad
  | KeyPressed String
  | ChangedPower
  | ChangedBank
  | ChangedVolume String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clicked { id } ->
      ( model
      , play (encodeSettings id model.volume)
      )

    KeyPressed key ->
      Debug.log key <|
        case String.uncons key of
          Just (c, "") ->
            case find (Char.toUpper c) .keyTrigger (toKit model.bank) of
              Just { id } ->
                ( model
                , if model.power then
                    play (encodeSettings id model.volume)
                  else
                    Cmd.none
                )

              Nothing ->
                ( model
                , Cmd.none
                )

          Just _ ->
            ( model
            , Cmd.none
            )

          Nothing ->
            ( model
            , Cmd.none
            )

    ChangedPower ->
      ( { model | power = not model.power }
      , Cmd.none
      )

    ChangedBank ->
      ( { model | bank = not model.bank }
      , Cmd.none
      )

    ChangedVolume newVolumeAsString ->
      ( case String.toInt newVolumeAsString of
          Just newVolume ->
            { model | volume = newVolume }

          Nothing ->
            model
      , Cmd.none
      )


encodeSettings : String -> Int -> Encode.Value
encodeSettings id volume =
  Encode.object
    [ ("id", Encode.string id)
    , ("volume", Encode.float (toFloat volume / 100))
    ]


-- COMMANDS


port play : Encode.Value -> Cmd msg


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  onKeyDown (Decode.map KeyPressed (Decode.field "key" Decode.string))


-- VIEW


view : Model -> Html Msg
view { power, bank, volume, display } =
  div []
    [ viewKit power (toKit bank)
    , viewSwitch "Power" "power" power ChangedPower
    , viewDisplay display
    , viewVolume volume
    , viewSwitch "Bank" "bank" bank ChangedBank
    ]


viewKit : Bool -> List DrumPad -> Html Msg
viewKit isEnabled kit =
  div [] (List.map (viewDrumPad isEnabled) kit)


viewDrumPad : Bool -> DrumPad -> Html Msg
viewDrumPad isEnabled pad =
  let
    src =
      "assets/audio/" ++ pad.id ++ ".mp3"
  in
    div [ A.title pad.name ]
      [ audio [ A.id pad.id, A.src src ] []
      , button
          [ A.disabled (not isEnabled)
          , E.onClick (Clicked pad)
          ]
          [ text (String.fromChar pad.keyTrigger) ]
      ]


viewDisplay : String -> Html msg
viewDisplay value =
  div [] [ text value ]


viewVolume : Int -> Html Msg
viewVolume level =
  div []
    [ input
        [ A.type_ "range"
        , A.min "0"
        , A.max "100"
        , A.value (String.fromInt level)
        , E.onInput ChangedVolume
        ]
        []
    ]


viewSwitch : String -> String -> Bool -> msg -> Html msg
viewSwitch title name value tag =
  div [ A.class "switch" ]
    [ h3 [] [ text title ]
    , label []
        [ input
            [ A.type_ "radio"
            , A.name name
            , A.checked (value == False)
            , E.onCheck (always tag)
            ]
            []
        , text "Off"
        ]
    , label []
        [ input
            [ A.type_ "radio"
            , A.name name
            , A.checked (value == True)
            , E.onCheck (always tag)
            ]
            []
        , text "On"
        ]
    ]


-- HELPERS


toKit : Bool -> List DrumPad
toKit bank =
  if bank then
    heaterKit
  else
    smoothPianoKit


find : b -> (a -> b) -> List a -> Maybe a
find key select values =
  case values of
    [] ->
      Nothing

    (v::vs) ->
      if key == select v then
        Just v
      else
        find key select vs


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

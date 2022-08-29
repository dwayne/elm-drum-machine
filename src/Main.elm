module Main exposing (main)


import Browser
import Html as H
import Json.Decode as JD
import Json.Encode as JE


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
  Result String State


type alias State =
  { bank : Bank
  }


type Bank
  = Bank1 Kit Kit
  | Bank2 Kit Kit


type alias Kit =
  { name : String
  , keyConfigs : List KeyConfig
  }


type alias KeyConfig =
  { id : String
  , name : String
  , key : Key
  }


type Key
  = Q
  | W
  | E
  | A
  | S
  | D
  | Z
  | X
  | C


init : Flags -> (Model, Cmd msg)
init value =
  let
    model =
      case JD.decodeValue bankDecoder value of
        Ok bank ->
          Ok { bank = bank }

        Err _ ->
          Err "Sorry, we're unable to start the application since it's not properly configured."
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


-- DECODER


bankDecoder : JD.Decoder Bank
bankDecoder =
  JD.map2 Bank1
    (JD.field "kit1" kitDecoder)
    (JD.field "kit2" kitDecoder)


kitDecoder : JD.Decoder Kit
kitDecoder =
  JD.map2 Kit
    (JD.field "name" JD.string)
    (JD.field "keyConfigs" <| JD.list keyConfigDecoder)


keyConfigDecoder : JD.Decoder KeyConfig
keyConfigDecoder =
  JD.map3 KeyConfig
    (JD.field "id" JD.string)
    (JD.field "name" JD.string)
    (JD.field "key" keyDecoder)


keyDecoder : JD.Decoder Key
keyDecoder =
  JD.string
    |> JD.andThen
        (\s ->
            case s of
              "Q" ->
                JD.succeed Q

              "W" ->
                JD.succeed W

              "E" ->
                JD.succeed E

              "A" ->
                JD.succeed A

              "S" ->
                JD.succeed S

              "D" ->
                JD.succeed D

              "Z" ->
                JD.succeed Z

              "X" ->
                JD.succeed X

              "C" ->
                JD.succeed C

              _ ->
                JD.fail <| "expected either Q, W, E, A, S, D, Z, X, or C: " ++ s
        )


-- VIEW


view : Model -> H.Html msg
view model =
  case model of
    Ok { bank } ->
      case bank of
        Bank1 kit _ ->
          H.text kit.name

        Bank2 _ kit ->
          H.text kit.name

    Err err ->
      H.text err


-- HELPERS


switchBank : Bank -> Bank
switchBank bank =
  case bank of
    Bank1 kit1 kit2 ->
      Bank2 kit1 kit2

    Bank2 kit1 kit2 ->
      Bank1 kit1 kit2

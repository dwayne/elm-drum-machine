module Main exposing (main)


import Bank exposing (Bank)
import Browser
import Html as H
import Html.Attributes as HA
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
      let
        kit =
          Bank.kit bank
      in
      H.text kit.name

    Nothing ->
      viewError "Sorry, we're unable to start the application since it's not properly configured."


viewError : String -> H.Html msg
viewError text =
  H.div
    [ HA.class "error" ]
    [ H.text text ]

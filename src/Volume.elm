module Volume exposing
  ( Volume
  , fromInt
  , toString, toValue
  , view
  )


import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE


type Volume
  = Volume Int


fromInt : Int -> Volume
fromInt n =
  Volume <| clamp 0 100 n


toString : Volume -> String
toString (Volume volume) =
  "Volume " ++ String.fromInt volume


toValue : Volume -> JE.Value
toValue (Volume volume) =
  JE.float (toFloat volume / 100)


view : (Volume -> msg) -> Bool -> Volume -> H.Html msg
view onInput isDisabled (Volume volume) =
  H.input
    [ HA.type_ "range"
    , HA.min "0"
    , HA.max "100"
    , HA.step "1"
    , HA.class "slider"
    , HA.disabled isDisabled
    , HA.value <| String.fromInt volume
    , customOnInput onInput
    ]
    []


customOnInput : (Volume -> msg) -> H.Attribute msg
customOnInput onInput =
  let
    decoder =
      HE.targetValue
        |> JD.andThen
            (\s ->
              case String.toInt s of
                Just n ->
                  JD.succeed <| onInput <| fromInt n

                Nothing ->
                  JD.fail ""
            )
  in
  HE.on "input" decoder

module Volume exposing
    ( Volume
    , fromInt
    , toString
    , toValue
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


fromString : String -> Maybe Volume
fromString =
    String.toInt >> Maybe.map fromInt


toString : Volume -> String
toString (Volume v) =
    String.fromInt v


toValue : Volume -> JE.Value
toValue (Volume v) =
    JE.float (toFloat v / 100)


view : (Volume -> msg) -> Bool -> Volume -> H.Html msg
view onVolume isDisabled volume =
    H.input
        [ HA.type_ "range"
        , HA.min "0"
        , HA.max "100"
        , HA.step "1"
        , HA.class "slider"
        , HA.value <| toString volume
        , if isDisabled then
            HA.disabled True

          else
            onVolumeInput onVolume
        ]
        []


onVolumeInput : (Volume -> msg) -> H.Attribute msg
onVolumeInput onVolume =
    let
        decoder =
            HE.targetValue
                |> JD.andThen
                    (\s ->
                        case fromString s of
                            Just volume ->
                                JD.succeed <| onVolume volume

                            Nothing ->
                                JD.fail "ignored"
                    )
    in
    HE.on "input" decoder

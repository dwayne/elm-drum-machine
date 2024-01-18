module Lib.Json.Decode exposing (fromMaybe)

import Json.Decode as JD


fromMaybe : Maybe a -> JD.Decoder a
fromMaybe ma =
    case ma of
        Just a ->
            JD.succeed a

        Nothing ->
            JD.fail "ignored"

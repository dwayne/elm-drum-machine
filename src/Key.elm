module Key exposing
    ( Key(..)
    , decoder
    , fromString
    , toString
    )

import Json.Decode as JD


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


fromString : String -> Maybe Key
fromString s =
    case String.toUpper s of
        "Q" ->
            Just Q

        "W" ->
            Just W

        "E" ->
            Just E

        "A" ->
            Just A

        "S" ->
            Just S

        "D" ->
            Just D

        "Z" ->
            Just Z

        "X" ->
            Just X

        "C" ->
            Just C

        _ ->
            Nothing


decoder : JD.Decoder Key
decoder =
    JD.string
        |> JD.andThen
            (\s ->
                case fromString s of
                    Just key ->
                        JD.succeed key

                    Nothing ->
                        JD.fail <| "expected either Q, W, E, A, S, D, Z, X, or C: " ++ s
            )


toString : Key -> String
toString key =
    case key of
        Q ->
            "Q"

        W ->
            "W"

        E ->
            "E"

        A ->
            "A"

        S ->
            "S"

        D ->
            "D"

        Z ->
            "Z"

        X ->
            "X"

        C ->
            "C"

module Key exposing
  ( Key(..)
  , decoder
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


decoder : JD.Decoder Key
decoder =
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

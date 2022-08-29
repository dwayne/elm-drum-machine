module Bank exposing
  ( Bank
  , Kit, KeyConfig, Key(..)
  , decoder
  , isBank2
  , kit
  , switch
  )


import Json.Decode as JD


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


decoder : JD.Decoder Bank
decoder =
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


isBank2 : Bank -> Bool
isBank2 bank =
  case bank of
    Bank2 _ _ ->
      True

    _ ->
      False


kit : Bank -> Kit
kit bank =
  case bank of
    Bank1 kit1 _ ->
      kit1

    Bank2 _ kit2 ->
      kit2


switch : Bank -> Bank
switch bank =
  case bank of
    Bank1 kit1 kit2 ->
      Bank2 kit1 kit2

    Bank2 kit1 kit2 ->
      Bank1 kit1 kit2

module Bank exposing
  ( Bank
  , Kit, KeyConfig
  , decoder
  , isBank2
  , kit
  , switch
  )


import Json.Decode as JD
import Key exposing (Key)


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
    (JD.field "key" Key.decoder)


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
